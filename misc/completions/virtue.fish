complete -c virtue -o h -d 'Print help'
complete -c virtue -o o -r -d 'Output to file'
complete -c virtue -o f -x -d 'Select format to generate' -a "\
    debug-ast\t\"Generate debug AST representation\"\
    debug-hir\t\"Generate debug HIR representation\"\
    c\t\"Generate C code\"\
    llvm-ir\t\"Generate LLVM IR\"\
    qbe-il\t\"Generate QBE IL\"\
    executable\t\"Generate executable\""
complete -c virtue -o b -x -d 'Select backend to use' -a "\
    c\t\"Use C backend\"\
    llvm\t\"Use LLVM backend\"\
    qbe\t\"Use QBE backend\""
