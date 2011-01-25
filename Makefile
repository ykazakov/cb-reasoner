SUBDIRS = src

.PHONY: main all clean install $(SUBDIRS)

main: $(SUBDIRS)

clean: 
	@$(MAKE) TARGET=$@

all: 
	@$(MAKE) TARGET=$@

$(SUBDIRS):
	@echo descending to $@
	@$(MAKE) -C $@ $(TARGET)

install:
	cp ./src/cb.native ./bin/cb
