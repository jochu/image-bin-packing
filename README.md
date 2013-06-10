# Image Bin Packing

This program does rectangle bin packing. It is intended to be used for generating packed sprite
sheets.

It is currently in an experimental state and generates decent packing.

## Usage

```sh
$ image-bin-packing input-images-dir output-image
```

`image-bin-packing` takes image specifications from stdin and outputs a series of ImageMagick
commands that will generate the packed image.

## Input Format

```
image width height
image' width' height'
...
```

## Output Format

```
# SHEET: width height
# image x y orientation
imagemagick-command
# image' x y orientation
imagemagick-command
...
```
