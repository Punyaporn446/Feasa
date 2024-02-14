BASIC INSTRUCTIONS

1. Please, read the manual first. Extended and useful information is provided there.
2. For UPDATING the DLL in existing projects: please, recheck function headers, since some might have been changed slightly
3. LIB files are not a static version of the DLL, but the files to be used in C-based compilers for importing all function headers.
4. Function headers and language-specific files with function declaractions can be found inside each language folder.
5. Please, ensure that the DLL file used matches with the computer/O.S. architecture used.
6. Ensure that compile options of your project have an architecture target type matching the DLL architecture used
7. Copy DLL files to c:\Windows\System32 or equivalent. DLL files can also be copied to the EXE folder.

Additional notes:
- examples in C#, VB.NET, Python and FreePascal are targeted to 64bit (used by 90% of programmers), while CVI and C are targeted to 32bit, but this can be easily adapted
- VB6 support and examples has been removed (deprecated)
- Python2 support and examples has been removed (deprecated)

(c) Feasa Enterprises Ltd.
