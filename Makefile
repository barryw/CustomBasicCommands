KICK=kickass

main.prg: main.asm
	$(KICK) main.asm -vicesymbols -bytedump -debugdump

run: clean main.prg
	x64sc main.prg

clean:
	rm -f main.prg
