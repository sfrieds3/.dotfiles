#!/bin/bash

BRIGHTNESS=$(light -G)

printf "%.0f%%" $BRIGHTNESS
