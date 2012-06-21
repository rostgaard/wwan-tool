default:
	gnatmake -P wwan_tool

clean:
	rm build/*	

distclean: clean
	rm wwan_tool
