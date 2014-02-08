#!/bin/sh
set -e
VOLNAME="PixyMon"
DMG="PixyMon.dmg"
SRC_DIR="../macbuild-pixymon-release/pixymon.app"
ICON_FILE="pixy.icns"

rm -rf ${DMG}
TMP_DMG="$(mktemp -u -t XXXXXXX)"
trap 'RESULT=$?; rm -f "$TMP_DMG"; exit $RESULT' INT QUIT TERM EXIT
hdiutil create -srcfolder "$SRC_DIR" -volname "$VOLNAME" -fs HFS+ \
               -fsargs "-c c=64,a=16,e=16" -format UDRW "$TMP_DMG"
TMP_DMG="${TMP_DMG}.dmg" # because OSX appends .dmg
DEVICE="$(hdiutil attach -readwrite -noautoopen "$TMP_DMG" | awk 'NR==1{print$1}')"
VOLUME="$(mount | grep "$DEVICE" | sed 's/^[^ ]* on //;s/ ([^)]*)$//')"
# start of DMG changes
cp "$ICON_FILE" "$VOLUME/.VolumeIcon.icns"
SetFile -c icnC "$VOLUME/.VolumeIcon.icns"
SetFile -a C "$VOLUME"
# end of DMG changes
hdiutil detach "$DEVICE"
hdiutil convert "$TMP_DMG" -format UDZO -imagekey zlib-level=9 -o "$DMG"
