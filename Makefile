build: public/index.html public/concrete.min.css src/**/**
	ln -s ../dopeloop.ai/live/public/zzfxloop ./build || true
	rsync -avL --exclude=js public/ build
	touch build
	npx shadow-cljs release --debug app
