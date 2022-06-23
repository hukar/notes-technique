# 30 Obtenir des infos sur un fichier

## `stat`

```bash
stat -x mypicture.jpeg
```

```bash
File: "mypicture.jpeg"
  Size: 571296       FileType: Regular File
  Mode: (0644/-rw-r--r--)         Uid: (  501/   hukar)  Gid: (   20/   staff)
Device: 1,8   Inode: 20102413    Links: 1
Access: Thu Apr 14 17:49:35 2022
Modify: Thu Apr 14 17:49:29 2022
Change: Thu Apr 14 17:49:33 2022
 Birth: Thu Apr 14 17:49:29 2022
```

Sur `Mac` il existe aussi `GetFileInfo` :

On aici la `Created Date` : Date de Création.

```bash
GetFileInfo myfile.png
```

```bash
file: "/Users/hukar/Desktop/myfile.png"
type: "\0\0\0\0"
creator: "\0\0\0\0"
attributes: avbstclinmEdz
created: 04/18/2022 09:27:39
modified: 04/18/2022 09:27:39
```



## Fichier `Image`

Les fichiers photo utilise une spécification pour les métadonnées :

`EXIF` : `Exchangeable Image File Format`.

Il existe des utilitaires en ligne de commande :

### `imagemagick`

```bash
brew install imagemagick
brew install ghostscript
```

```bash
identify -verbose /Users/hukar/Desktop/move-cursor-for-change-value.png 
```

```bash
Image:
  Filename: /Users/hukar/Desktop/move-cursor-for-change-value.png
  Format: PNG (Portable Network Graphics)
  Mime type: image/png
  Class: DirectClass
  Geometry: 1336x570+0+0
  Resolution: 144x144
  Print size: 9.27778x3.95833
  Units: PixelsPerInch
  Colorspace: sRGB
  Type: TrueColorAlpha
  Base type: Undefined
  Endianness: Undefined
  Depth: 8-bit
  Channel depth:
    Red: 8-bit
    Green: 8-bit
    Blue: 8-bit
    Alpha: 1-bit
  Channel statistics:
    Pixels: 761520
    Red:
      min: 10  (0.0392157)
      max: 255 (1)
      mean: 56.4426 (0.221344)
      median: 49 (0.192157)
      standard deviation: 39.4874 (0.154852)
      kurtosis: 17.9622
      skewness: 4.29336
      entropy: 0.374264
    Green:
      min: 10  (0.0392157)
      max: 204 (0.8)
      mean: 64.8588 (0.254348)
      median: 77 (0.301961)
      standard deviation: 22.1229 (0.0867566)
      kurtosis: 6.89344
      skewness: 1.45383
      entropy: 0.399024
    Blue:
      min: 10  (0.0392157)
      max: 204 (0.8)
      mean: 87.2439 (0.342133)
      median: 121 (0.47451)
      standard deviation: 39.6041 (0.15531)
      kurtosis: -1.54446
      skewness: -0.078829
      entropy: 0.393403
    Alpha:
      min: 255  (1)
      max: 255 (1)
      mean: 255 (1)
      median: 255 (1)
      standard deviation: 0 (0)
      kurtosis: 8.192e+51
      skewness: 1e+36
      entropy: 0
  Image statistics:
    Overall:
      min: 10  (0.0392157)
      max: 255 (1)
      mean: 115.886 (0.454456)
      median: 125.5 (0.492157)
      standard deviation: 25.3036 (0.0992298)
      kurtosis: -1.06043
      skewness: 0.81634
      entropy: 0.291673
  Rendering intent: Perceptual
  Gamma: 0.454545
  Chromaticity:
    red primary: (0.64,0.33)
    green primary: (0.3,0.6)
    blue primary: (0.15,0.06)
    white point: (0.3127,0.329)
  Matte color: grey74
  Background color: white
  Border color: srgb(223,223,223)
  Transparent color: none
  Interlace: None
  Intensity: Undefined
  Compose: Over
  Page geometry: 1336x570+0+0
  Dispose: Undefined
  Iterations: 0
  Compression: Zip
  Orientation: Undefined
  Profiles:
    Profile-exif: 144 bytes
    Profile-icc: 3400 bytes
    Profile-xmp: 449 bytes
  Properties:
    date:create: 2022-04-18T07:29:43+00:00
    date:modify: 2022-04-18T07:28:33+00:00
    exif:ExifOffset: 78
    exif:PixelXDimension: 1336
    exif:PixelYDimension: 570
    exif:UserComment: ASCII...Screenshot
    icc:copyright: Copyright Apple Inc., 2022
    icc:description: Display
    png:iCCP: chunk was found
    png:IHDR.bit-depth-orig: 8
    png:IHDR.bit_depth: 8
    png:IHDR.color-type-orig: 6
    png:IHDR.color_type: 6 (RGBA)
    png:IHDR.interlace_method: 0 (Not interlaced)
    png:IHDR.width,height: 1336, 570
    png:pHYs: x_res=5669, y_res=5669, units=1
    signature: faae4cc0aa38317032bc4dd20b100a291d0a8bb9ba41114fd64b3cad1bcdee69
    xmp:PixelXDimension: 1336
    xmp:PixelYDimension: 570
    xmp:UserComment: Screenshot
  Artifacts:
    verbose: true
  Tainted: False
  Filesize: 104491B
  Number pixels: 761520
  Pixels per second: 27.184MP
  User time: 0.040u
  Elapsed time: 0:01.028
  Version: ImageMagick 7.1.0-29 Q16-HDRI x86_64 19841 https://imagemagick.org
```

