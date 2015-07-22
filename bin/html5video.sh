#!/bin/bash

# For this to work, you need to have ffmpeg
# installed with libvorbis, theora and libvpx ENABLED
# to do that in Homebrew:
# brew reinstall ffmpeg --with-libvpx --with-libvorbis --with-theora
#
# encoding reference:
# https://blog.mediacru.sh/2013/12/23/The-right-way-to-encode-HTML5-video.html

VIDSOURCE="${1}"

function convert {
    VEXT="${1}"
    VCODEC=""
    ACODEC=""
    SIZE="${2}"
    VBITRATE="${3}"
    ABITRATE="${4}"
    FRAMERATE="${5}"
    OUTPUT="v_${SIZE}_${VBITRATE}.${VEXT}"

    PROFILE=""

    if [[ "${SIZE}" == "240" ]]; then
        SIZE="320x240"
    elif [[ "${SIZE}" == "360" ]]; then
        SIZE="640x360"
    elif [[ "${SIZE}" == "480" ]]; then
        SIZE="854x480"
    elif [[ "${SIZE}" == "720" ]]; then
        SIZE="1280x720"
    elif [[ "${SIZE}" == "1080" ]]; then
        SIZE="1920x1080"
    fi

    if [[ "${VEXT}" == "mp4" ]]; then
        VCODEC="libx264"
        ACODEC="libfaac"
        PROFILE="-profile:v baseline"
    elif [[ "${VEXT}" == "webm" ]]; then
        VCODEC="libvpx"
        ACODEC="libvorbis"
    elif [[ "${VEXT}" == "ogg" ]]; then
        VCODEC="libtheora"
        ACODEC="libvorbis"
    fi

    ffmpeg -y -i ${VIDSOURCE} -c:v ${VCODEC} -s:v ${SIZE} -r:v ${FRAMERATE} -b:a ${ABITRATE}k -ac 2 -c:a ${ACODEC} -b:v ${VBITRATE}k -maxrate ${VBITRATE}k -bufsize ${VBITRATE}k ${PROFILE} ${OUTPUT}
}

# 360p
convert webm 360 1000 98 24
convert webm 360 850 98 24
convert webm 360 700 98 24
convert ogg 360 1000 98 24
convert ogg 360 850 98 24
convert ogg 360 700 98 24
convert mp4 360 1000 98 24
convert mp4 360 850 98 24
convert mp4 360 700 98 24

# 480p
convert webm 480 1450 128 24
convert webm 480 1300 128 24
convert webm 480 1000 128 24
convert ogg 480 1450 128 24
convert ogg 480 1300 128 24
convert ogg 480 1000 128 24
convert mp4 480 1450 128 24
convert mp4 480 1300 128 24
convert mp4 480 1000 128 24

# 720p
convert webm 720 1900 160 24
convert webm 720 1750 160 24
convert webm 720 1600 160 24
convert ogg 720 1900 160 24
convert ogg 720 1750 160 24
convert ogg 720 1600 160 24
convert mp4 720 1900 160 24
convert mp4 720 1750 160 24
convert mp4 720 1600 160 24
