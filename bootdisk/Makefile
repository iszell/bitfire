OS = $(shell uname)

ifeq ($(OS),Windows_NT)
  ACME = ../acme/src/acme
  D64W = ../bitfire/d64write
  LZ = ../bitfire/bitnax
else
  OSS = $(shell uname -o)
  ACME = acme
  ifeq ($(OSS),Android)
    D64W = ~/bitfire/bitfire/d64write
    LZ = ~/bitfire/bitfire/bitnax
  else
    D64W = ../bitfire/d64write
    LZ = ../bitfire/bitnax
  endif
endif

ACMEOPT = -f cbm

ifdef HEADER
YOUR_HEADER = "$(HEADER)"
else
YOUR_HEADER = "hello"
endif

ifdef ID
YOUR_ID = "$(ID)"
else
YOUR_ID = "world"
endif

all:
	make -C . START_ADDR=100d bootdisks
	make -C . START_ADDR=2000 bootdisks
	make -C . START_ADDR=4000 bootdisks
	
clean:
	rm boot_loadcomp_*.d64 boot boot.prg || true

bootdisks : boot_loadcomp_one_g$(START_ADDR).d64 boot_loadcomp_two_g$(START_ADDR).d64

boot_loadcomp_one_g$(START_ADDR).d64 : boot.asm Makefile 
	$(ACME) $(ACMEOPT) -o boot.prg -DLOAD=1 -DSTART=0x$(START_ADDR) boot.asm
	$(LZ) --sfx 4352 --plus4 -o boot boot.prg
	#$(D64W) -c $@ -h $(YOUR_HEADER) -i $(YOUR_ID) --side 1 --boot boot
	$(D64W) -c $@ -a 17 dirart.prg --side 1 --boot boot
	rm boot boot.prg
	
boot_loadcomp_two_g$(START_ADDR).d64 : boot.asm Makefile 
	$(ACME) $(ACMEOPT) -o boot.prg -DLOAD=2 -DSTART=0x$(START_ADDR) boot.asm
	$(LZ) --sfx 4352 --plus4 -o boot boot.prg
	#$(D64W) -c $@ -h $(YOUR_HEADER) -i $(YOUR_ID) --side 1 --boot boot
	$(D64W) -c $@ -a 17 dirart.prg --side 1 --boot boot
	rm boot boot.prg
