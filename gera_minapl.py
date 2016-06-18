#!/usr/bin/env python3

import os
import sys
import random
import itertools

di = [-1, -1, -1, 0, 1, 1, 1, 0]
dj = [-1,  0,  1, 1, 1, 0, -1, -1]

if __name__ == '__main__':
    if len(sys.argv) < 3:
        print('Uso:\n./{} tamanho minas'.format(
            os.path.basename(__file__)))
        exit(0)

    n, mines = map(int, sys.argv[1:])

    if mines >= (n * n):
        print('ERRO: Mais minas do que casas.')
        exit(0)

    mine_cells = set()

    for _ in itertools.repeat(None, mines):
        while True:
            i = random.randint(0, n-1)
            j = random.randint(0, n-1)

            if (i, j) not in mine_cells:
                mine_cells.add((i, j))
                break

    f = open('mina.pl', 'w+')
    f.write('tamanho({}).\n'.format(n))
    for i, j in sorted(mine_cells):
        f.write('mina({},{}).\n'.format(i+1, j+1))

    f.close()

    print('\n   ', end='')
    for j in range(n):
        print(' {:2} '.format(j+1), end='')

    print('\n    {}'.format('-' * (n * 4 - 1)))
    for i in range(n):
        print('{:2} |'.format(i+1), end='')
        for j in range(n):
            if (i, j) in mine_cells:
                print(' ▨ |', end='')
            else:
                around = 0
                for k in range(8):
                    if (i+di[k], j+dj[k]) in mine_cells:
                        around += 1
                if around == 0:
                    around = ' '
                print(' {} |'.format(around), end='')

        if i < n-1:
            print('\n   |{}'.format('--- ' * (n-1)), end='')
            print('---|')
        else:
            print('\n    {}\n'.format('-' * (n * 4 - 1)))
        #
    #
