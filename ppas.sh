#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Assembling ureader
/usr/bin/clang -c -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/ureader.o'  -arch i386 -mmacosx-version-min=10.8 -x assembler '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/ureader.s'
if [ $? != 0 ]; then DoExitAsm ureader; fi
rm '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/ureader.s'
echo Assembling utoken
/usr/bin/clang -c -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/utoken.o'  -arch i386 -mmacosx-version-min=10.8 -x assembler '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/utoken.s'
if [ $? != 0 ]; then DoExitAsm utoken; fi
rm '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/utoken.s'
echo Assembling uerror
/usr/bin/clang -c -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uerror.o'  -arch i386 -mmacosx-version-min=10.8 -x assembler '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uerror.s'
if [ $? != 0 ]; then DoExitAsm uerror; fi
rm '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uerror.s'
echo Assembling ulexer
/usr/bin/clang -c -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/ulexer.o'  -arch i386 -mmacosx-version-min=10.8 -x assembler '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/ulexer.s'
if [ $? != 0 ]; then DoExitAsm ulexer; fi
rm '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/ulexer.s'
echo Assembling uast
/usr/bin/clang -c -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uast.o'  -arch i386 -mmacosx-version-min=10.8 -x assembler '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uast.s'
if [ $? != 0 ]; then DoExitAsm uast; fi
rm '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uast.s'
echo Assembling uvisitor
/usr/bin/clang -c -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uvisitor.o'  -arch i386 -mmacosx-version-min=10.8 -x assembler '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uvisitor.s'
if [ $? != 0 ]; then DoExitAsm uvisitor; fi
rm '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uvisitor.s'
echo Assembling uprinter
/usr/bin/clang -c -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uprinter.o'  -arch i386 -mmacosx-version-min=10.8 -x assembler '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uprinter.s'
if [ $? != 0 ]; then DoExitAsm uprinter; fi
rm '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uprinter.s'
echo Assembling umemory
/usr/bin/clang -c -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uMemory.o'  -arch i386 -mmacosx-version-min=10.8 -x assembler '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uMemory.s'
if [ $? != 0 ]; then DoExitAsm umemory; fi
rm '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uMemory.s'
echo Assembling umembers
/usr/bin/clang -c -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uMembers.o'  -arch i386 -mmacosx-version-min=10.8 -x assembler '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uMembers.s'
if [ $? != 0 ]; then DoExitAsm umembers; fi
rm '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uMembers.s'
echo Assembling ucallable
/usr/bin/clang -c -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uCallable.o'  -arch i386 -mmacosx-version-min=10.8 -x assembler '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uCallable.s'
if [ $? != 0 ]; then DoExitAsm ucallable; fi
rm '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uCallable.s'
echo Assembling ufunc
/usr/bin/clang -c -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uFunc.o'  -arch i386 -mmacosx-version-min=10.8 -x assembler '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uFunc.s'
if [ $? != 0 ]; then DoExitAsm ufunc; fi
rm '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uFunc.s'
echo Assembling uarrayintf
/usr/bin/clang -c -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uArrayIntf.o'  -arch i386 -mmacosx-version-min=10.8 -x assembler '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uArrayIntf.s'
if [ $? != 0 ]; then DoExitAsm uarrayintf; fi
rm '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uArrayIntf.s'
echo Assembling udictintf
/usr/bin/clang -c -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uDictIntf.o'  -arch i386 -mmacosx-version-min=10.8 -x assembler '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uDictIntf.s'
if [ $? != 0 ]; then DoExitAsm udictintf; fi
rm '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uDictIntf.s'
echo Assembling ustandard
/usr/bin/clang -c -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/ustandard.o'  -arch i386 -mmacosx-version-min=10.8 -x assembler '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/ustandard.s'
if [ $? != 0 ]; then DoExitAsm ustandard; fi
rm '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/ustandard.s'
echo Assembling uclassintf
/usr/bin/clang -c -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uClassIntf.o'  -arch i386 -mmacosx-version-min=10.8 -x assembler '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uClassIntf.s'
if [ $? != 0 ]; then DoExitAsm uclassintf; fi
rm '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uClassIntf.s'
echo Assembling uclass
/usr/bin/clang -c -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uClass.o'  -arch i386 -mmacosx-version-min=10.8 -x assembler '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uClass.s'
if [ $? != 0 ]; then DoExitAsm uclass; fi
rm '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uClass.s'
echo Assembling uvariantsupport
/usr/bin/clang -c -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uVariantSupport.o'  -arch i386 -mmacosx-version-min=10.8 -x assembler '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uVariantSupport.s'
if [ $? != 0 ]; then DoExitAsm uvariantsupport; fi
rm '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uVariantSupport.s'
echo Assembling uarray
/usr/bin/clang -c -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uArray.o'  -arch i386 -mmacosx-version-min=10.8 -x assembler '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uArray.s'
if [ $? != 0 ]; then DoExitAsm uarray; fi
rm '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uArray.s'
echo Assembling uresolver
/usr/bin/clang -c -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uResolver.o'  -arch i386 -mmacosx-version-min=10.8 -x assembler '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uResolver.s'
if [ $? != 0 ]; then DoExitAsm uresolver; fi
rm '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uResolver.s'
echo Assembling ulanguage
/usr/bin/clang -c -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/ulanguage.o'  -arch i386 -mmacosx-version-min=10.8 -x assembler '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/ulanguage.s'
if [ $? != 0 ]; then DoExitAsm ulanguage; fi
rm '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/ulanguage.s'
echo Assembling udict
/usr/bin/clang -c -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uDict.o'  -arch i386 -mmacosx-version-min=10.8 -x assembler '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uDict.s'
if [ $? != 0 ]; then DoExitAsm udict; fi
rm '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uDict.s'
echo Assembling uenumintf
/usr/bin/clang -c -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uEnumIntf.o'  -arch i386 -mmacosx-version-min=10.8 -x assembler '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uEnumIntf.s'
if [ $? != 0 ]; then DoExitAsm uenumintf; fi
rm '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uEnumIntf.s'
echo Assembling uenum
/usr/bin/clang -c -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uEnum.o'  -arch i386 -mmacosx-version-min=10.8 -x assembler '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uEnum.s'
if [ $? != 0 ]; then DoExitAsm uenum; fi
rm '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uEnum.s'
echo Assembling umath
/usr/bin/clang -c -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/umath.o'  -arch i386 -mmacosx-version-min=10.8 -x assembler '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/umath.s'
if [ $? != 0 ]; then DoExitAsm umath; fi
rm '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/umath.s'
echo Assembling utupleintf
/usr/bin/clang -c -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/utupleintf.o'  -arch i386 -mmacosx-version-min=10.8 -x assembler '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/utupleintf.s'
if [ $? != 0 ]; then DoExitAsm utupleintf; fi
rm '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/utupleintf.s'
echo Assembling utuple
/usr/bin/clang -c -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/utuple.o'  -arch i386 -mmacosx-version-min=10.8 -x assembler '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/utuple.s'
if [ $? != 0 ]; then DoExitAsm utuple; fi
rm '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/utuple.s'
echo Assembling uinterpreter
/usr/bin/clang -c -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uinterpreter.o'  -arch i386 -mmacosx-version-min=10.8 -x assembler '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uinterpreter.s'
if [ $? != 0 ]; then DoExitAsm uinterpreter; fi
rm '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uinterpreter.s'
echo Assembling uparser
/usr/bin/clang -c -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uparser.o'  -arch i386 -mmacosx-version-min=10.8 -x assembler '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uparser.s'
if [ $? != 0 ]; then DoExitAsm uparser; fi
rm '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/uparser.s'
echo Assembling gear
/usr/bin/clang -c -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/gear.o'  -arch i386 -mmacosx-version-min=10.8 -x assembler '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/gear.s'
if [ $? != 0 ]; then DoExitAsm gear; fi
rm '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/lib/i386-darwin/gear.s'
echo Linking /Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/gear
OFS=$IFS
IFS="
"
/usr/bin/ld       -dead_strip -no_dead_strip_inits_and_terms -x  -order_file '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/symbol_order.fpc' -multiply_defined suppress -L. -o '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/gear' `cat '/Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/link.res'`  -pagezero_size 0x10000
if [ $? != 0 ]; then DoExitLink /Users/Jeroen/Documents/Programming/ProjectGearLang/Ch16-Enhancements copy/gear; fi
IFS=$OFS
