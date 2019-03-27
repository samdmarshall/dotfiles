
TARGET = dotfiles

SUBDIRS =    \
	hero       \
	notmuch    \
	vdirsyncer

MKDIR_PV = mkdir --parents --verbose

export

all: $(SUBDIRS)

$(SUBDIRS):
	cd $@ && $(MAKE) $(MAKECMDGOALS) $(MAKEFLAGS)

.PHONY: all $(SUBDIRS)
