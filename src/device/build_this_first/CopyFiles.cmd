@Echo Off
set sourcedir="D:\Users\adevries\sources\GitHub\pixy\src"
copy %sourcedir%\device\common\src\*.*  %sourcedir%\device\libpixy_m0\src
copy %sourcedir%\device\common\src\*.*  %sourcedir%\device\libpixy_m4\src
copy %sourcedir%\common\src\*.*  %sourcedir%\device\libpixy_m4\src
pause