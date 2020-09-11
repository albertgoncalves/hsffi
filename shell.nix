with import <nixpkgs> {};
mkShell {
    buildInputs = [
        clang_10
        cppcheck
        ghc
        glibcLocales
        hlint
        ormolu
        shellcheck
    ];
    shellHook = ''
        . .shellhook
    '';
}
