@rem @set PATH=c:/cygwin-x86_64/tmp/cmake-3.0.2-win32-x86/bin;%PATH%
@set PATH=c:/cygwin-x86_64/tmp/cmake-3.1.0-win32-x86/bin;%PATH%


@del /Q CMakeCache.txt
@del /Q cmake_install.cmake
@rmdir /Q /S CMakeFiles
@rmdir /Q /S clang-server-x86_64.dir


cmake -G "Visual Studio 12 2013 Win64" ../clang-server
@rem cmake -G "Visual Studio 12 2013 Win64" ../clang-server -DCMAKE_INSTALL_PREFIX="c:/cygwin-x86_64/usr/local/bin/"
@rem cmake -G "Visual Studio 12 2013 Win64" ../clang-server -DLIBRARY_PATHS="c:/cygwin-x86_64/tmp/llvm-build-shells/ps1/clang-350/build/msvc-64/"
@rem cmake -G "Visual Studio 12 2013 Win64" ../clang-server -DLIBRARY_PATHS="c:/cygwin-x86_64/tmp/llvm-build-shells/ps1/clang-350/build/msvc-64/" -DCMAKE_INSTALL_PREFIX="c:/cygwin-x86_64/usr/local/bin/"
@pause

@rem cmake --build . [--config <config>] [--target <target>] [-- -i]
@rem cmake --build . --config Release --target ALL_BUILD
@rem cmake --build . --config Debug --target ALL_BUILD
cmake --build . --config Release --target INSTALL
@rem cmake --build . --config Debug --target INSTALL

@pause
