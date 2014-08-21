.SUFFIXES:

# For testing, without Cargo setting this.
OUT_DIR ?= $(realpath .)

export V = false
export PREFIX = @
ifeq ($(V),true)
	PREFIX =
endif

SNAPPY_VERSION  := 1.1.2
LEVELDB_VERSION := 1.15.0

SNAPPY_DIR := deps/snappy-$(SNAPPY_VERSION)
LEVELDB_DIR := deps/leveldb-$(LEVELDB_VERSION)


# ----------------------------------------------------------------------

all:


libs_for_cargo: snappy leveldb
	$(PREFIX)cp $(SNAPPY_LIBRARY) $(OUT_DIR)/libsnappy.a
	$(PREFIX)cp $(LEVELDB_LIBRARY) $(OUT_DIR)/libleveldb.a


# ----------------------------------------------------------------------

SNAPPY_LIBRARY := $(SNAPPY_DIR)/.libs/libsnappy.a

.PHONY: snappy
snappy: $(SNAPPY_LIBRARY)

$(SNAPPY_LIBRARY): $(SNAPPY_DIR)/Makefile
	$(PREFIX)make -C $(SNAPPY_DIR)

$(SNAPPY_DIR)/Makefile: $(SNAPPY_DIR)/config.status

$(SNAPPY_DIR)/config.status:
	$(PREFIX)cd $(SNAPPY_DIR) && ./configure CXXFLAGS="-fPIC"


# ----------------------------------------------------------------------

LEVELDB_LIBRARY := $(LEVELDB_DIR)/libleveldb.a

.PHONY: leveldb
leveldb: $(LEVELDB_LIBRARY)

$(LEVELDB_LIBRARY): snappy
	$(PREFIX)LDFLAGS="-L$(realpath $(SNAPPY_DIR))/.libs" CXXFLAGS="-I$(realpath $(SNAPPY_DIR)) -fPIC" make -C $(LEVELDB_DIR)


# ----------------------------------------------------------------------

.PHONY: clean
clean:
	$(PREFIX)-make -C $(SNAPPY_DIR) distclean
	$(PREFIX)-make -C $(LEVELDB_DIR) clean
	$(PREFIX)cargo clean
