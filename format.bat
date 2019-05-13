@echo off
set astyle="D:\Embedded\AStyle\bin\astyle.exe"
for /r . %%a in (*.cpp;*.c) do %astyle% --style=linux  -n "%%a"
for /r . %%a in (*.hpp;*.h) do %astyle% --style=linux  -n "%%a"
for /r . %%a in (*.orig) do del "%%a"
exit
