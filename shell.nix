{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, attoparsec, base, distributive, OpenGLRaw, lens
      , linear, random, sdl2, stdenv, text, transformers, JuicyPixels
      , wavefront, reactive-banana, clock
      }:
      mkDerivation {
        pname = "ssao-example";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base distributive OpenGLRaw lens linear random sdl2 text
          transformers JuicyPixels wavefront reactive-banana clock
        ];
        homepage = "https://github.com/ocharles/ssao-example";
        description = "A demonstration of screen-space ambient occlusion using OpenGL & Haskell";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = with pkgs; (haskellPackages.override {
    overrides = self: super: {
      OpenGL = self.callPackage
        ({ mkDerivation, base, bytestring, GLURaw, ObjectName, OpenGLRaw, StateVar, text }:
        mkDerivation {
          pname = "OpenGL";
          version = "3.0.0.0";
          sha256 = "1w2vn2xjnr7cciknlvr4486n2hi82jdacni9kwvkgn7y02w7cnph";
          libraryHaskellDepends = [ base bytestring GLURaw ObjectName OpenGLRaw StateVar text ];
          homepage = "http://www.haskell.org/haskellwiki/Opengl";
          description = "A binding for the OpenGL graphics system";
          license = stdenv.lib.licenses.bsd3;
          hydraPlatforms = stdenv.lib.platforms.none;
        }) {};

      OpenGLRaw = self.callPackage
        ({ mkDerivation, base, fixed, half, ghc-prim, mesa, text }:
        mkDerivation {
          pname = "OpenGLRaw";
          version = "3.0.0.0";
          sha256 = "1wdbisgjsajlpq622ap9n0h4dc92wgimjnzrfylwbpdr1g4c9vw1";
          libraryHaskellDepends = [ base ghc-prim fixed half text ];
          librarySystemDepends = [ mesa ];
          homepage = "http://www.haskell.org/haskellwiki/Opengl";
          description = "A raw binding for the OpenGL graphics system";
          license = stdenv.lib.licenses.bsd3;
          hydraPlatforms = stdenv.lib.platforms.none;
        }) {inherit (pkgs) mesa;};

      GLURaw = self.callPackage
        ({ mkDerivation, base, freeglut, mesa, OpenGLRaw }:
        mkDerivation {
          pname = "GLURaw";
          version = "2.0.0.0";
          sha256 = "014i3mi66yc2g1yik3wfynh6mxkzw75xrlkcdim8yrmnr8dmvpcd";
          libraryHaskellDepends = [ base OpenGLRaw ];
          librarySystemDepends = [ freeglut mesa ];
          homepage = "http://www.haskell.org/haskellwiki/Opengl";
          description = "A raw binding for the OpenGL graphics system";
          license = stdenv.lib.licenses.bsd3;
          hydraPlatforms = stdenv.lib.platforms.none;
        }) {inherit (pkgs) freeglut; inherit (pkgs) mesa;};

      "linear" = self.callPackage
        ({ mkDerivation, adjunctions, base, binary, bytes, bytestring
         , cereal, containers, deepseq, directory, distributive, doctest
         , filepath, ghc-prim, hashable, HUnit, lens, reflection
         , semigroupoids, semigroups, simple-reflect, tagged
         , template-haskell, test-framework, test-framework-hunit
         , transformers, transformers-compat, unordered-containers, vector
         , void
         }:
         mkDerivation {
           pname = "linear";
           version = "1.20.3";
           sha256 = "50f63a5b6019acb53ae06886749dea80443b18876c2990ca5376578c94537ac4";
           libraryHaskellDepends = [
             adjunctions base binary bytes cereal containers deepseq
             distributive ghc-prim hashable lens reflection semigroupoids
             semigroups tagged template-haskell transformers transformers-compat
             unordered-containers vector void
           ];
           testHaskellDepends = [
             base binary bytestring directory doctest filepath HUnit lens
             simple-reflect test-framework test-framework-hunit
           ];
           homepage = "http://github.com/ekmett/linear/";
           description = "Linear Algebra";
           license = stdenv.lib.licenses.bsd3;
         }) {};

      wavefront = self.callPackage
        ({ mkDerivation, attoparsec, base, dlist, filepath, mtl, text
         , transformers, vector
         }:
         mkDerivation {
           pname = "wavefront";
           version = "0.6";
           sha256 = "1v4r26zv79cgq7324wys6dm6qj2w8wskfk8i930i52kvggigsasp";
           libraryHaskellDepends = [
             attoparsec base dlist filepath mtl text transformers vector
           ];
           homepage = "https://github.com/phaazon/wavefront";
           description = "Wavefront OBJ loader";
           license = stdenv.lib.licenses.bsd3;
         }) {};
    };}).callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
