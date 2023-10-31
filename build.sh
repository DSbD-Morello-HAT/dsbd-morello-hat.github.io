#!/bin/bash
stack build && stack exec dsbd-morello-hat clean && stack exec dsbd-morello-hat build
