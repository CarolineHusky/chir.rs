{
  parallel,
  stdenv,
  imagemagick,
  oxipng,
  jpegoptim,
  lotte-art,
}:
stdenv.mkDerivation {
  pname = "art-encode";
  inherit (lotte-art) version;

  src = lotte-art;

  nativeBuildInputs = [parallel imagemagick oxipng jpegoptim];

  buildPhase = ''
    # First, we convert all source images to png, due to a silly jxl thing where we can’t reencode jxl

    parallel -j$NIX_BUILD_CORES mogrify -verbose -format png ::: *.jxl
    rm -v *.jxl

    # Optimize all pngs
    oxipng *.png # TODO: -o max -Z?

    # TODO: tweak these numbers, currently heifs and avifs are larger than webps

    # Create 80% jpegs
    parallel -j$NIX_BUILD_CORES mogrify -verbose -format jpg -quality 80 ::: *.png
    jpegoptim *.jpg *.jpeg

    # Create 80% webps
    parallel -j$NIX_BUILD_CORES mogrify -verbose -format webp -quality 80 ::: *.png

    # Create 60% heifs
    parallel -j$NIX_BUILD_CORES mogrify -verbose -format heif -quality 60 ::: *.png

    # Create 85% avifs
    parallel -j$NIX_BUILD_CORES mogrify -verbose -format avif -quality 85 ::: *.png

    # Create 70% jxls
    parallel -j$NIX_BUILD_CORES mogrify -verbose -format jxl -quality 70 ::: *.png
  '';
  installPhase = ''
    mkdir $out
    mv *.png *.jpg *.webp *.heif *.avif *.jxl $out
  '';
}
