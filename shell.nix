{ system ? builtins.currentSystem
, compiler ? "default"
}:

(builtins.getFlake (toString ./.)).devShells.${system}.${compiler}
