# ASM build script.
# Joe Norton, September 2021, Oxford Brookes University.
# Syntax for building and linking asm file:
# build_asm_vX.sh <source_file>.asm
# Version 8: Colour formatting added to output.

#!/bin/bash
INPUT_FILE=$@ # use this line to take input file name from the command line
#INPUT_FILE="test.asm" # use this line to hard-code an input file name

#Define colours to change the output text colours
red="\e[1;31m"
thin_red="\e[0;31m"
blue="\e[1;34m"
thin_blue="\e[0;34m"
green="\e[1;32m"
thin_green="\e[0;32m"
reset_colour="\e[0m"

OBJECT_FILE_NAME=$(basename $INPUT_FILE .asm).o # for <name>.asm source file, assembles to object file <name>.o
OUTPUT_EXECUTABLE_NAME=$(basename $INPUT_FILE .asm) # for <name>.o object file, links to executable filename <name>
USE_YASM=0 #set to 1 if you want to use YASM for assembly instead of NASM
USE_LD=0 #set to 1 to use LD for linking instead of GCC
USE_lasm_io_SWITCH=1

# Set the object path name if you want to manually specify where the object code archive is.
# You should be able to leave this blank if you are using the -lasm_io switch, specified above.
LIB_ASM_OBJECT_FILE_PATH=""
#LIB_ASM_OBJECT_FILE_PATH="/usr/lib/libasm_io.a"


if [ $USE_YASM = 0 ]; then # using nasm for assembling
    ASSEMBLER_ARGS="-g -f elf64" #nasm args
else # using yasm for assembling
    ASSEMBLER_ARGS="-g dwarf2 -f elf64" #yasm args
fi

LINKER_ARGS="-no-pie"
if [ $USE_lasm_io_SWITCH = 1 ]; then
    LINKER_ARGS="-lasm_io $LINKER_ARGS"
fi

printf "Source file to build: ${thin_blue}$INPUT_FILE${reset_colour}\n"

if [ "$INPUT_FILE" = "$OUTPUT_EXECUTABLE_NAME" ]; then
    printf "${red}Input file and output files are the same. Ensure that source file has a .asm extension.${reset_colour}\n"
    exit 1
fi
if [ "$INPUT_FILE" = "$OBJECT_FILE_NAME" ]; then
    printf "${red}Input file and output files are the same. Ensure that source file has a .asm extension.${reset_colour}\n"
    exit 1
fi

#generate assembly command
if [ $USE_YASM = 0 ]; then # using nasm for assembling
    printf "Using ${thin_green}nasm${reset_colour} for assembly.\n"
    COMMAND_TO_RUN="nasm $ASSEMBLER_ARGS -o $OBJECT_FILE_NAME $INPUT_FILE"
else # using yasm for assembling
    printf "Using ${thin_green}yasm${reset_colour} for assembly.\n"
    COMMAND_TO_RUN="yasm $ASSEMBLER_ARGS -o $OBJECT_FILE_NAME $INPUT_FILE"
fi
#run assembler command
printf "Running assembler command:\n${thin_red}$COMMAND_TO_RUN${reset_colour}\n"
$COMMAND_TO_RUN

#link (only if assembler was successful)
if [ $? -eq 0 ]; then # Assemble stage was successful (return value of yasm/nasm is zero)
    printf "${green}Success: ${reset_colour}Assembler executed successfully.\nOutputted object file is: ${thin_blue}$OBJECT_FILE_NAME${reset_colour}\nLinking...\n"
    if [ $USE_LD = 0 ]; then # using gcc for linking
    printf "Using using ${thin_green}gcc${reset_colour} for linking.\n"
        COMMAND_TO_RUN="gcc $OBJECT_FILE_NAME $LIB_ASM_OBJECT_FILE_PATH $LINKER_ARGS -o $OUTPUT_EXECUTABLE_NAME"
    else # using ld for linking
        printf "Using using ${thin_green}ld${reset_colour} for linking.\n"
        COMMAND_TO_RUN="ld $OBJECT_FILE_NAME $LIB_ASM_OBJECT_FILE_PATH $LINKER_ARGS -o $OUTPUT_EXECUTABLE_NAME"
    fi
    #run link command
    printf "Running linking command:\n${thin_red}$COMMAND_TO_RUN${reset_colour}\n"
    $COMMAND_TO_RUN
    
    if [ $? -eq 0 ]; then # Linking stage was successful (return value of gcc/ld is zero)
        printf "${green}Success: ${reset_colour}Linker executed successfully.${reset_colour}\nOutputted executable file name is: ${thin_blue}$OUTPUT_EXECUTABLE_NAME${reset_colour}\n"
    else # Linking stage was failed (return value of gcc/ld is non-zero)
        printf "${red}Warning: ${reset_colour}Something went wrong with linker. No executable file has been created.\n"
    fi
else  # Assemble stage failed (return value of yasm/nasm is non-zero)
    printf "${red}Warning:${reset_colour} Something went wrong with assembler. Linker stage will not be executed. No object file or executable file has been created.\n"
fi
