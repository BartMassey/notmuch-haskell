# notmuch-haskell: notmuch MUA Haskell binding high-level interface
# Copyright © 2010 Bart Massey
# Licensed LGPL v3: please see the file COPYING in this
# source distribution for licensing information.

OBJS = Notmuch.o Notmuch.hi NOTMUCH_H_hsc.o
LIBS = -lgmime-2.4 -lz -lnsl -lgobject-2.0 -lglib-2.0 -ltalloc -lxapian
INCLUDES = -Inotmuch/lib
NOTMUCHLIB = notmuch/lib/libnotmuch.a

notmuchtest: $(OBJS) NotmuchTest.hs
	ghc --make -o notmuchtest NotmuchTest.hs $(NOTMUCHLIB) $(LIBS)

NOTMUCH_H_hsc.c NOTMUCH_H_hsc.h NOTMUCH_H.hs: NOTMUCH_H.hsc
	hsc2hs $(INCLUDES) NOTMUCH_H.hsc

NOTMUCH_H_hsc.o: NOTMUCH_H_hsc.c NOTMUCH_H_hsc.h
	ghc --make $(INCLUDES) NOTMUCH_H_hsc.c

NOTMUCH_H.o NOTMUCH_H.hi: NOTMUCH_H.hs
	ghc --make $(INCLUDES) NOTMUCH_H.hs

Notmuch.o Notmuch.hi: Notmuch.hs NOTMUCH_H_hsc.o NOTMUCH_H.hi NOTMUCH_H.o
	ghc --make Notmuch.hs

clean:
	-rm -f $(OBJS) NOTMUCH_H_hsc.[ch] NOTMUCH_H_stub.[cho] \
	  NOTMUCH_H.o NOTMUCH_H.hs NOTMUCH_H.hi \
          NotmuchTest.o NotmuchTest.hi \
          notmuchtest
