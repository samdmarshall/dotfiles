TARGET = notmuch

MAILBOX_DEST = $(HOME)/eMail

STEPS = create-mailbox install-hooks

export

all: $(TARGET)

$(TARGET):
	$(MKDIR_PV) $(MAILBOX_DEST)
	$(MKDIR_PV) $(HOME)/eMail-backup

	$(MAKE) MAILBOX_NAME=Inbox  create-mailbox
	$(MAKE) MAILBOX_NAME=Outbox create-mailbox
	$(MAKE) MAILBOX_NAME=Drafts create-mailbox
	$(MAKE) install-hooks

create-mailbox:
	cd $(MAILBOX_DEST) && \
		$(MKDIR_PV) $(MAILBOX_NAME)

	cd $(MAILBOX_DEST)/$(MAILBOX_NAME) && \
		$(MKDIR_PV) .notmuch/hooks cur new tmp

install-hooks:
	cp --no-clobber --verbose --target-directory=$(MAILBOX_DEST)/Inbox/.notmuch/hooks \
		./hooks/pre-new ./hooks/post-new

.PHONY: all $(STEPS)
