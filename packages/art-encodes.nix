{
  stdenv,
  imagemagick,
  lotte-art,
  emptyDirectory,
  lib,
  lndir,
}: let
  mkPng = {
    name,
    size,
    ...
  }:
    stdenv.mkDerivation {
      name = "${name}-${toString size}.png";
      inherit (lotte-art) version;
      src = emptyDirectory;

      nativeBuildInputs = [imagemagick];

      unpackPhase = ''
        ln -sv ${lotte-art}/${name}.jxl in.jxl
      '';

      buildPhase = ''
        convert in.jxl -resize ${toString size}x${toString size} out.png
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

  mkImg' = {
    name,
    ext,
    size,
  }:
    stdenv.mkDerivation {
      name = "${name}-${toString size}.${ext}";
      inherit (lotte-art) version;
      src = emptyDirectory;

      nativeBuildInputs = [imagemagick];
      input = mkPng {inherit name size;};

      unpackPhase = ''
        ln -sv $input/${name}-${toString size}.png in.png
      '';

      buildPhase = ''
        convert in.png -quality ${qualities.${ext}} out.${ext}
      '';

      installPhase = ''
        mkdir $out
        mv out.${ext} $out/$name
      '';
    };

  mkImg = args:
    if args.ext == "png"
    then mkPng args
    else mkImg' args;

  flatmap = f: xs: lib.flatten (map f xs);

  mkImgs = {
    name,
    sizes,
  }:
    flatmap (ext:
      map (size: mkImg {inherit name size ext;}) sizes) ["png" "jpg" "webp" "heif" "avif" "jxl"];

  imgSet = flatmap mkImgs [
    {
      name = "2023-10-26-sammythetanuki-babylottepfp";
      sizes = [128 256];
    }
  ];
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
