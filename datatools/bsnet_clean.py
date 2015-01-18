#!/usr/bin/env python
# coding: utf-8
# Data manipulation tool about BS network data
# By chenxm
# chenxm35@gmail.com
import sys

def main():
    if len(sys.argv) < 3:
        print("Usage: bsnet_clean.py <cell.map> <outfile>")
        sys.exit(-1)
    cell_map = sys.argv[1]
    outfile = sys.argv[2]

    i = 0
    of = open(outfile, 'wb')
    for line in open(cell_map, 'rb'):
        line = line.strip('\r\n ')
        if len(line) == 0 or line[0] == '#':
            continue
        i += 1
        parts = line.split('\t')
        if len(parts) != 5:
            print("Invalid line [%d]: %s" % (i, line))
            continue
        lac, ci, lon, lat, des = parts

        # generate unique BS identifier
        try:
            bs = long(lac) * 1000000 + long(ci)
        except:
            print("Invalid LAC/CI: %s" % line)
            continue

        # identify city
        city = -1
        if '杭州' in des:
            city = 0
        elif '温州' in des:
            city = 1
        of.write('\t'.join([str(city), str(bs), lon, lat]))
        of.write('\n')
    of.close()

    print("Total: %d" % i)



if __name__ == '__main__':
    main()
