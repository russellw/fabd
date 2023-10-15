clang-format -i --style=file *.cc||exit /b
sort-c -i *.cc||exit /b
sort-cases -i *.cc||exit /b
git diff
