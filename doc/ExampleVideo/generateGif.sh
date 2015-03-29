#!/usr/bin/env bash
# This script generates a gif file from PixeloVideo.mkv

mplayer -ss 0 -endpos 11 PixeloVideo.mkv -vo png:z=1:outdir=2vid -ao null
# Remove any unwanted frames. Mplayer only can only cut at the next nearest
# keyframe (every 250th frame here), so we can't get more precision with this
# tool.
mkdir 3vid
for f in 2vid/00000*; do g=${f#2vid}; convert -resize 50% $f 3vid$g; done
convert +repage -fuzz 1.6% -delay 8 -loop 0 3vid/*.png -layers OptimizePlus -layers OptimizeTransparency Almost.gif
gifsicle -O3 --colors 256 Almost.gif > PixeloVideo.gif
rm -r 2vid 3vid Almost.gif
