#!/bin/bash

while inotifywait -e close_write main.coco
do
  coconut main.coco --no-tco
done
