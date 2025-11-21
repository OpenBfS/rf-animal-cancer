#!/bin/bash

# Create target directories
mkdir -p male_schwannoma female_schwannoma m+f_schwannoma male_glioma female_glioma m+f_glioma

# Move schwannoma files
mv mal*_schwa* schwa* male_schwannoma 2>/dev/null
mv fem*_schwa* female_schwannoma 2>/dev/null
mv m+f_schwa* m+f_schwannoma 2>/dev/null
mv m-vs-f_schwa* m+f_schwannoma 2>/dev/null

# Move glioma files
mv mal*_gli* gli* male_glioma 2>/dev/null
mv fem*_gli* female_glioma 2>/dev/null
mv m+f_gli* m+f_glioma 2>/dev/null
mv m-vs-f_gli* m+f_glioma 2>/dev/null

mv */*.xlsx .