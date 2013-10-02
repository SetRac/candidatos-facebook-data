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

test:
	csvfix check -nl -v data/csv/*.csv

.PHONY: test
