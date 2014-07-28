#!/bin/bash

java -cp JAPI.jar JAPI &
JAPIPID=$!

echo $JAPIPID > JAPI.pid
