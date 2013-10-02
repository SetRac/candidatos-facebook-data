data/%:

data/json/candidatos-facebook.json: data/csv/candidatos-facebook.csv scripts/csv2json.js
	node scripts/csv2json $< \
		> $@

data/csv/candidatos-facebook.csv:
	curl \
		"https://docs.google.com/spreadsheet/pub?key=0AueNVGWJxTnOdEltRjc1LVBBb2lKQzgwb01yemthUVE&single=true&gid=0&output=csv" \
		--compressed \
		--progress \
		--fail \
	| \
	csvfix \
		exclude -f 1 \
	> \
	$@

scripts/csv2json.js: node_modules

node_modules: package.json
	npm install
	touch $@

test:
	csvfix check -nl -v data/csv/*.csv

.PHONY: test
