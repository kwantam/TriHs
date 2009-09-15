## Copyright (C) 2009 Riad S Wahby <rsw@jfet.org>
## 
## This file is part of QDkae
##
##  QDkae is free software.  It comes without any warranty, to
##  to the extent permitted by applicable law.  You can redistribute it
##  and/or modify it under the terms of the Do What The Fuck You Want To
##  Public License, Version 2, as published by Sam Hocevar.  See
##  the COPYING file or http://sam.zoy.org/wtfpl/COPYING for more details
##

all: TriHs

TriHs: TriHs.hs TriHsPieces.hs
	ghc -O2 --make TriHs.hs

clean:
	rm -rf *.o *.hi TriHs
