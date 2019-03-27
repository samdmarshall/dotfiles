TARGET = vdirsyncer

STEPS = setup-dirs

export

all: $(TARGET)

$(TARGET): $(STEPS)

setup-dirs:
	cd $(HOME) && \
		$(MKDIR_PV) Calendars Contacts Tasks

.PHONY: all $(STEPS)
