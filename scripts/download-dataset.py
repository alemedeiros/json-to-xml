#!/usr/bin/env python3

# download-dataset.py
#   by alemedeiros <alexandre.n.medeiros _at_ gmail.com>
#
# Script to download an artist dataset from musicbrainz and save it as json
# files.
#
# How to use:
#   ./dowload-dataset.py <file>
#
# Where the file must have an artist name per line.
# The script then queries the musicbrainz API for a search with each artist name
# and then downloads the information on all the artists returned by the search
# query.

import json
import musicbrainzngs
import sys

# Get input file
if len(sys.argv) != 2:
    print("missing file with bands name", file=sys.stderr)
    exit(1)
f = open(sys.argv[1], 'r')

# Set musicbrainz api library
musicbrainzngs.set_useragent("data downloading app", "0.1", "http://alemedeiros.sdf.org")


for l in f.read().splitlines():
    # Query for artist info
    res = musicbrainzngs.search_artists(l)

    # Get the data for all the artists returned
    for art in res['artist-list']:
        art_data = musicbrainzngs.get_artist_by_id(art['id'], includes=['aliases', 'tags', 'ratings'])

        # Save the data for each artist in a different json file
        jf = open(art['id'] + '.json', 'w')
        jf.write(json.dumps(art_data['artist'], sort_keys=True, indent=2))
