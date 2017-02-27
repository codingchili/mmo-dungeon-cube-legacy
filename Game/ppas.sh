#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Linking /home/rduda/dev/Dgc Project/Game/lib/x86_64-linux/Game.or
OFS=$IFS
IFS="
"
/usr/bin/fpcres -o "/home/rduda/dev/Dgc Project/Game/lib/x86_64-linux/Game.or" -a x86_64 -of elf -v "@/home/rduda/dev/Dgc Project/Game/lib/x86_64-linux/Game.reslst"
if [ $? != 0 ]; then DoExitLink /home/rduda/dev/Dgc Project/Game/lib/x86_64-linux/Game.or; fi
IFS=$OFS
echo Linking Game
OFS=$IFS
IFS="
"
/usr/bin/ld -b elf64-x86-64 -m elf_x86_64  --dynamic-linker=/lib64/ld-linux-x86-64.so.2    -L. -o Game link.res
if [ $? != 0 ]; then DoExitLink Game; fi
IFS=$OFS
