/*
 * Written by Aleksander Gaweda
 * 2020
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * V1.5 Initial commit to GitHub
 * V1.6
 * -
 * 
 * ToDo:
 * -Add doxygen documentation
 * -Add support for custom polynomial, custom fill value and specific CRC write address (parsing
 *      number from input)
 * -Add support for calculating CRC of binary file
 * -Add convert options (hex->bin/bin->hex)
 */

/*Includes*/
#include <vector>
#include <string>
#include <fstream>
#include <iostream>
#include <iso646.h>
#include <inttypes.h>

using namespace std;

/*Global variables*/
uint32_t poly = 0x04C11DB7;
uint16_t fillment = 0xFFFF;

/*New data types*/
typedef enum ProgramStatus : int {
    success = 0,
    noParam = -1,
    fileExtErr = -2,
    unknownFlag = -3,
    noInputFile = -4,
    lineCRCerr = -5,
    unsupportedFeature = -6,
    openingFileErr = -7,
} ProgramStatus;

typedef union {
    struct {
        char colon;
        char byteCount[2];
        char address[4];    //in big-endian
        char recordType[2];
        char data[32];
        char checksum[2];   //position can change if line contains less data bytes
        char newLine;       //same as above
    } __attribute__((packed));
    char array[44];         //in proper file, line shouldn't be longer
} __attribute__((packed)) IntelHexASCII;

typedef enum RecordType : uint8_t {
    addr16bitAndData  = 0,
    endOfFile  = 1,
    addr20bit  = 2,
    start20bit = 3,
    addr32bit  = 4,
    start32bit = 5,
} RecordType;

typedef union {
    struct {
        uint8_t byteCount;
        union {
            uint8_t byte[2];
            uint16_t half;
        } __attribute__((packed)) address;
        RecordType recordType;
        union {
            uint8_t byte[16];
            uint16_t half[8];
            uint32_t word[4];
        } __attribute__((packed)) data;
        uint8_t checksum;
    } __attribute__((packed));
    uint8_t array[21];
} __attribute__((packed)) IntelHexBinary;

typedef union {
    struct {
        uint16_t:4;
        uint32_t ext20bit:4;
        uint16_t:12;
    } __attribute__((packed));
    struct {
        uint16_t:16;
        uint16_t ext32bit:16;
    } __attribute__((packed));
    struct {
        uint16_t base:16;
        uint16_t:16;
    } __attribute__((packed));
    uint32_t word;
} Address;

template <typename T>
union Caster {
    T reg;
    uint8_t byte[sizeof(T)];
} __attribute__((packed));

/*Functions*/
template <typename T>
T charHex2uint( const char *str )
{
	T ret = 0;
	for( register uint_fast8_t x=0; x<sizeof(T)<<1; x++ )
	{
		ret <<= 4;
		if( str[x] >= '0' and str[x] <= '9' )
			ret |= (str[x] - 48);
		else if( str[x] >= 'A' and str[x] <= 'F' )
			ret |= (str[x] - 55);
		else if( str[x] >= 'a' and str[x] <= 'f' )
			ret |= (str[x] - 87);
		else
			break;
	}
	return ret;
}

template <typename T>
T charDec2uint( const char *str )
{
    T ret = 0;
	register uint_fast32_t pos = 0;

	while( isdigit( str[pos] ) )
	{
		ret*=10;
		ret += (str[pos] - '0');
		pos++;
	}

	return ret;
}

template <typename T>
bool getNum( const char *str, T &num )
{
    size_t v = 0, f = 0, x = 0, d = 0;
    bool isHex = false;

    for( ; ; v++ )
    {
        if( str[v] == '\0' )
        {
            if( f == v )
                return false; //number not found
            if( x > d )
                isHex = true;
            break;
        }

        if( str[v] == '0' and str[v+1] == 'x' and isxdigit( str[v+2] ) )
        {
            isHex = true;
            f = v + 2;
            break;
        }

        if( isdigit( str[v] ) ) //0-9
            d++, x++;
        else if( isxdigit( str[v] ) ) //a-f and A-F
            x++;
        else //any other
            f++;
    }

    if( isHex )
        num = charHex2uint<T>( &str[f] );
    else
        num = charDec2uint<T>( &str[f] );
    return true;
}

template <typename T>
char *uint2charHex( T val, char *buf, size_t len )
{
    if( len < sizeof(T)*2 )
        return nullptr;

    size_t pos = sizeof(T)*2;

    if( len >= sizeof(T)*2 + 1 )
        buf[pos] = '\0';

    pos--;

    uint8_t mask;

    while( 1 )
    {
        mask = val & 0xF;
        if( mask < 10 )
            buf[pos] = mask + '0';
        else
            buf[pos] = mask - 10 + 'A';

        if( pos == 0 )
            break;
        else
            pos--;

        val>>=4;
    }
    return buf;
}

template <typename T>
T switchEndian( T in )
{
    Caster<T> a, b;
    a.reg = in;
    uint8_t temp;

    for( register uint_fast8_t e=0; e<sizeof(T); e++ )
    {
        temp = a.byte[e];
        a.byte[e] = b.byte[sizeof(T)-1-e];
        b.byte[sizeof(T)-1-e] = temp;
    }

    return b.reg;
}

uint8_t calcIntelHexCRC( IntelHexBinary *bin )
{
    uint16_t crc = 0;

    crc += bin->byteCount;
    crc += bin->address.byte[0];
    crc += bin->address.byte[1];
    crc += (uint8_t) bin->recordType;
    for( register uint_fast8_t d=0; d<bin->byteCount; d++ )
        crc += bin->data.byte[d];
    crc &= 0xFF;
    if( crc != 0 )
        crc = 0x100 - crc;

    return crc;
}

bool convertToBinary( IntelHexASCII *ascii, IntelHexBinary *bin )
{
    if( ascii->colon != ':' )
        return false;

    bin->byteCount       = charHex2uint<uint8_t>( ascii->byteCount );
    bin->address.byte[0] = charHex2uint<uint8_t>( &ascii->address[2] );
    bin->address.byte[1] = charHex2uint<uint8_t>( &ascii->address[0] );
    bin->recordType = (RecordType) charHex2uint<uint8_t>( ascii->recordType );

    for( register uint_fast8_t c=0; c<bin->byteCount; c++ )
        bin->data.byte[c] = charHex2uint<uint8_t>( &ascii->data[c<<1] );

    if( bin->byteCount == 16 )
        bin->checksum = charHex2uint<uint8_t>( ascii->checksum );
    else
        bin->checksum = charHex2uint<uint8_t>( &ascii->data[(bin->byteCount<<1)] );

    /*Calculate line checksum*/
    if( calcIntelHexCRC( bin ) != bin->checksum )
        return false;

    return true;
}

bool convertToText( IntelHexBinary *bin, IntelHexASCII *ascii )
{
    for( size_t s=0; s<sizeof(IntelHexASCII); s++ )
        ascii->array[s] = '\0';

    ascii->colon = ':';
    uint2charHex<uint8_t>( bin->byteCount, ascii->byteCount, 2 );
    uint2charHex<uint8_t>( bin->address.byte[0], &ascii->address[2], 2 );
    uint2charHex<uint8_t>( bin->address.byte[1], &ascii->address[0], 2 );
    uint2charHex<uint8_t>( (uint8_t) bin->recordType, ascii->recordType, 2 );
    for( uint8_t c=0; c<bin->byteCount; c++ )
        uint2charHex<uint8_t>( bin->data.byte[c], &ascii->data[c*2], 2 );

    uint8_t crc = calcIntelHexCRC( bin );

    if( bin->byteCount == 16 )
    {
        uint2charHex<uint8_t>( crc, ascii->checksum, 2 );
        ascii->newLine = '\0';
    }
    else
    {
        uint2charHex<uint8_t>( crc, &ascii->data[bin->byteCount*2], 2 );
        ascii->data[bin->byteCount*2+2] = '\0';
    }

    return true;
}

uint32_t calculateDataCRC( uint32_t init, uint32_t data ) //init is previous value calculated or 0xFFFFFFFF if first calculation
{
    uint32_t buf = init^data;
    for( register uint_fast8_t b=0; b<32; b++ )
        buf & (1<<31) ? (buf = (buf<<1)^poly) : (buf<<=1);
    return buf;
}

int main( int argc, char **argv )
{
    const string help(  "-i <name>    Input file name (automatically interpreted as .hex if extension omitted)\n"\
                        "-o <name>    Output file name; if not specified, input file is over-written (only when -w specified)\n"\
                        "-w <addr>    Write calculated CRC to file, it is written to last 4B of data or at address <addr> (truncates data from this location)\n"\
                        "-f <fill>    fill empty spaces with value <fill> (default 0xFFFF; only when -w specified)\n"\
                        "-p <poly>    Custom polynomial (default 0x04C11DB7)\n"\
                        "-h           Display this help\n" );

    /*Variables*/
    string inputFileName;
    string outputFileName;
    ifstream inputFile;
    ofstream outputFile;
    bool argWriteCRC = false;
    uint32_t lineCnt;
    IntelHexASCII lineASCII;
    IntelHexBinary lineInteger;
    Address currentAddress;
    Address firstAddress;
    Address crcWriteAddress;
    bool extAddrInitDone = false;
    bool baseAddrInitDone = false;
    uint8_t pgmData[1048576]; //1MB
    uint32_t pgmDataIdx = 0;
    uint32_t result = 0xFFFFFFFF; //last result from crc calculation
    ProgramStatus ret = success;

    if( argc == 1 )
    {
        cout << "No parameters" << endl;
        ret = noParam;
        goto end;
    }

    /*Search for parameters in passed arguments*/
    for( int_fast32_t s=1; s<argc; s++ )
    {
        if( argv[s][0] == '-' ) //parameters always start with a dash
        {
            switch( argv[s][1] )
            {
            case 'i':
                if( argc >= s+1 )
                {
                    s++;

                    inputFileName.append( argv[s] );

                    /*Check for extension*/
                    if( inputFileName.find( ".hex" ) == string::npos ) //no proper extension
                    {
                        if( inputFileName.find( '.' ) == string::npos ) //no extension part at all
                        {
                            inputFileName.append( ".hex" );
                        }
                        else //other extension
                        {
                            cout << "Unsupported input file extension, only .hex is supported" << endl;
                            ret = fileExtErr;
                            goto end;
                        }
                    }
                    break;
                }
                else
                {
                    cout << "No value for -i flag" << endl;
                    ret = noParam;
                    goto end;
                }

            case 'o':
                if( argc >= s+1 )
                {
                    s++;
                    outputFileName.append( argv[s] );

                    /*Check for extension*/
                    if( outputFileName.find( ".hex" ) == string::npos ) //no proper extension
                    {
                        if( outputFileName.find( '.' ) == string::npos ) //no extension part at all
                        {
                            outputFileName.append( ".hex" );
                        }
                        else //other extension
                        {
                            cout << "Unsupported output file extension, only .hex is supported" << endl;
                            ret = fileExtErr;
                            goto end;
                        }
                    }
                    break;
                }
                else
                {
                    cout << "No value for -o flag" << endl;
                    ret = noParam;
                    goto end;
                }

            case 'w':
                //ToDo: custom CRC write address (parameter should be optional!)
                //s++;
                argWriteCRC = true;
                break;

            case 'f':
                s++;
                if( getNum( argv[s], fillment ) == true )
                {
                    cout << "Using custom fill value: 0x" << hex << uppercase << fillment << endl;
                }
                else
                {
                    cout << "Parameter not valid or not specified" << endl;
                }
                break;

            case 'p':
                s++;
                if( getNum( argv[s], poly ) == true )
                {
                    cout << "Using custom polynomial: 0x" << hex << uppercase << poly << endl;
                }
                else
                {
                    cout << "Parameter not valid or not specified" << endl;
                }
                break;

            case 'h':
                cout << help << endl;
                goto end;

            default:
                cout << "Unknown flag -" << argv[s][1] << endl;
                ret = unknownFlag;
                goto end;
            }
        }
        else
        {
            cout << "Extra parameter " << argv[s] << endl;
        }
    }

    if( inputFileName.length() == 0 )
    {
        cout << "Input file not specified" << endl;
        ret = noInputFile;
        goto end;
    }

    /*Open input file*/
    inputFile.open( inputFileName, ios::in );

    /*Read input file*/
    if( inputFile.is_open() )
    {
        lineCnt = 0xFFFFFFFF;
        currentAddress.word = 0x00000000;
        firstAddress.word = 0x00000000;

        /*Read text line, convert to binary and store in vector*/
        while( inputFile.getline( lineASCII.array, sizeof(IntelHexASCII) ) ) //gets data till \n or fills whole buffer
        {
            lineCnt++;
            if( convertToBinary( &lineASCII, &lineInteger ) != true )
            {
                cout << "Error converting line " << dec << lineCnt + 1 << ':' << "\n\t" << lineASCII.array << endl;
                ret = lineCRCerr;
                goto end;
            }

            /*Control records*/
            if( lineInteger.recordType == RecordType::endOfFile )
                break;
            else if( lineInteger.recordType == RecordType::start20bit )
                continue;
            else if( lineInteger.recordType == RecordType::start32bit )
                continue;

            /*Fetch address from address records*/
            if( lineInteger.recordType == RecordType::addr16bitAndData )
            {
                currentAddress.base = lineInteger.address.half;
                if( !baseAddrInitDone )
                {
                    firstAddress.base = lineInteger.address.half;
                    baseAddrInitDone = true;
                }
            }
            else if( lineInteger.recordType == RecordType::addr20bit )
            {
                cout << "20bit address space not yet supported" << endl;
                ret = unsupportedFeature;
                break;
            }
            else if( lineInteger.recordType == RecordType::addr32bit )
            {
                currentAddress.ext32bit = lineInteger.data.byte[0]<<8 | lineInteger.data.byte[1];
                if( !extAddrInitDone )
                {
                    firstAddress.ext32bit = lineInteger.data.byte[0]<<8 | lineInteger.data.byte[1];
                    extAddrInitDone = true;
                }
                continue;
            }

            /*Fill gaps to maintain address continuity*/
            bool filledGap = false;
            while( firstAddress.word + pgmDataIdx < currentAddress.word )
            {
                pgmData[pgmDataIdx] = 0xFF;
                pgmDataIdx++;
                filledGap = true;
            }
            if( filledGap )
                cout << "Filled gap" << endl;

            /*Copy data from data records*/
            if( lineInteger.recordType == RecordType::addr16bitAndData )
            {
                for( uint8_t c=0; c<lineInteger.byteCount; c++ )
                {
                    pgmData[pgmDataIdx] = lineInteger.data.byte[c];
                    pgmDataIdx++;
                }
            }
        }
        inputFile.close();

        if( ret == unsupportedFeature )
            goto end;
    }
    else
    {
        cout << "Can not open file " << inputFileName << endl;
        ret = openingFileErr;
        goto end;
    }

    /*Fill array to multiple of 4*/
    if( pgmDataIdx%4 != 0 )
    {
        for( uint8_t x=0; x<pgmDataIdx%4; x++ )
        {
            pgmData[pgmDataIdx] = 0xFF;
            pgmDataIdx++;
        }
        cout << "Aligned" << endl;
    }

//    cout << dec << pgmDataIdx << endl;

    /*Calculate CRC from array*/
    for( uint32_t i=0; i<pgmDataIdx-4; i+=4 )
    {
        uint32_t temp = pgmData[i+3]<<24 | pgmData[i+2]<<16 | pgmData[i+1]<<8 | pgmData[i+0];
        result = calculateDataCRC( result, temp );
    }

    /*Write CRC result to last 4 bytes*/
    cout << "CRC = 0x" << hex << uppercase << switchEndian<uint32_t>(result) << endl;
    Caster<uint32_t> cast;
    cast.reg = result;
    pgmData[pgmDataIdx-4] = cast.byte[0]; //pgmDataIdx points to first unwritten location
    pgmData[pgmDataIdx-3] = cast.byte[1];
    pgmData[pgmDataIdx-2] = cast.byte[2];
    pgmData[pgmDataIdx-1] = cast.byte[3];

    /*Write data to file*/
    if( argWriteCRC )
    {
        if( outputFileName.length() != 0 )
            outputFile.open( outputFileName, ios::in | ios::trunc ); //write to output file
        else
            outputFile.open( inputFileName, ios::in | ios::trunc ); //write to input file

        if( !outputFile.is_open() )
        {
            cout << "Error opening file " << outputFileName << endl;
            ret = openingFileErr;
            goto end;
        }

        uint32_t w=0;
        IntelHexASCII iha;
        IntelHexBinary ihb;
        Address wrtAddr;
        uint8_t bytes;
        bool writeExtAddr = true;

        while( 1 )
        {
            /*RecordType::endOfFile*/
            if( w >= pgmDataIdx )
            {
                ihb.byteCount = 0;
                ihb.address.half = 0;
                ihb.recordType = RecordType::endOfFile;
                convertToText( &ihb, &iha );
                outputFile << iha.array << '\n';
                break;
            }

            /*RecordType::addr32bit*/
            if( writeExtAddr )
            {
                ihb.byteCount = 2;
                ihb.address.half = 0;
                ihb.recordType = RecordType::addr32bit;
                wrtAddr.word = firstAddress.word + w;
                ihb.data.half[0] = wrtAddr.ext32bit;
                ihb.data.half[0] = switchEndian( ihb.data.half[0] );
                convertToText( &ihb, &iha );
                outputFile << iha.array << '\n';
                writeExtAddr = false;
            }

            /*RecordType::addr16bitAndData*/
            bytes = (w+16 <= pgmDataIdx) ? 16 : (uint8_t) pgmDataIdx - w;
            ihb.byteCount = bytes;
            wrtAddr.word = firstAddress.word + w;
            ihb.address.half = wrtAddr.base;
            ihb.recordType = RecordType::addr16bitAndData;
            for( uint8_t n=0; n<bytes; n++ )
                ihb.data.byte[n] = pgmData[w+n];
            convertToText( &ihb, &iha );
            outputFile << iha.array << '\n';
            w += bytes;

            wrtAddr.word = firstAddress.word + w;
            if( wrtAddr.base == 0 )
                writeExtAddr = true;
        }

        outputFile.close();
    }

    cout << "Done" << endl;
end:
    return ret;
}
