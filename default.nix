{ system ? builtins.currentSystem
}:

(builtins.getFlake (toString ./.)).packages.${system}.default
