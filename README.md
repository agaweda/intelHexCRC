# intelHexCRC
Calculate CRC of Intel Hex file contents


# Compile
g++ -std=gnu++11 main.cpp -o intelHexCRC.exe


# Integration with Atollic TrueSTUDIO
The main purpose I wrote this program, was calculating CRC of
.hex file generated by Atollic TrueSTUDIO. It was intended to
be invoked in post build phase, but later it turned out, that
convert-hex.jar, which is responsible for converting to Intel
HEX, is executed after the post build phase. To overcome this,
what You have to do, is to invoke convert-hex.jar manually in
post build. In Windows, command may look like this:
"C:\Program Files (x86)\Atollic\TrueSTUDIO for STM32 9.3.0\ide\jre\bin\java" -jar "C:\Program Files (x86)\Atollic\TrueSTUDIO for STM32 9.3.0\Tools\arm-atollic-reports.jar" convert-hex PROGRAM.elf
This command will and generate PROGRAM.hex
Then we execute our converter with command similar to this:
C:\Users\agaweda\Desktop\intelHexCRC\intelHexCRC.exe -w -i PROGRAM.hex
On windows, to invoke multiple commands in post-build, we have to
separete them with an ampersand '&', on linux with a semicolon ';'.
