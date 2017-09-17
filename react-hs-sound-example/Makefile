INSTALL_ROOT:=$(shell stack path --allow-different-user --local-install-root)

default: js-build/install-root js-build/sound.js
all: npm stack default

.PHONY:

stack: .PHONY
	stack build --allow-different-user --fast

npm: .PHONY
	npm install

js-build/install-root: $(INSTALL_ROOT)
	mkdir -p js-build
	ln -sf $(INSTALL_ROOT) js-build/install-root

js-build/sound.js: $(INSTALL_ROOT)/bin/sound.jsexe/all.js
	mkdir -p js-build
	echo "(function(global,React,ReactDOM) {" > js-build/sound.js
	cat $(INSTALL_ROOT)/bin/sound.jsexe/all.js >> js-build/sound.js
	echo "})(window, window['React'], window['ReactDOM']);" >> js-build/sound.js
	sed -i 's/goog.provide.*//' js-build/sound.js
	sed -i 's/goog.require.*//' js-build/sound.js

clean:
	rm -rf js-build
