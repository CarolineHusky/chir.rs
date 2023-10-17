{
  stdenv,
  imagemagick,
  lotte-art,
  emptyDirectory,
  lib,
  lndir,
}: let
  mkPng = name:
    stdenv.mkDerivation {
      name = name + ".png";
      inherit (lotte-art) version;
      src = emptyDirectory;

      nativeBuildInputs = [imagemagick];

      unpackPhase = ''
        ln -sv ${lotte-art}/${name}.jxl in.jxl
      '';

      buildPhase = ''
        convert in.jxl out.png
      '';

      installPhase = ''
        mkdir $out
        mv out.png $out/$name
      '';
    };

  qualities = {
    jpg = "80";
    webp = "80";
    heif = "60";
    avif = "85";
    jxl = "70";
  };

  mkImg' = name: ext:
    stdenv.mkDerivation {
      name = name + ".${ext}";
      inherit (lotte-art) version;
      src = emptyDirectory;

      nativeBuildInputs = [imagemagick];
      input = mkPng name;

      unpackPhase = ''
        ln -sv $input/${name}.png in.png
      '';

      buildPhase = ''
        convert in.png -quality ${qualities.${ext}} out.${ext}
      '';

      installPhase = ''
        mkdir $out
        mv out.${ext} $out/$name
      '';
    };

  mkImg = name: ext:
    if ext == "png"
    then mkPng name
    else mkImg' name ext;

  mkImgs = name: map (i: mkImg name i) ["png" "jpg" "webp" "heif" "avif" "jxl"];

  flatmap = f: xs: lib.flatten (map f xs);

  imgSet = flatmap mkImgs ["2023-06-02-vintagecoyote-prideicon"];
in
  stdenv.mkDerivation {
    pname = "art-encode";
    inherit (lotte-art) version;

    src = emptyDirectory;

    nativeBuildInputs = [lndir];

    buildPhase = "";
    installPhase = ''
      mkdir $out
      for f in ${toString imgSet}; do
        lndir $f $out
      done
    '';
  }
