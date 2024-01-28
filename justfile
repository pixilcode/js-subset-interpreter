generate_input_js_tree:
	for file in `ls test/input_js/*.js`; do \
		echo "${file} -> ${file}.acorn"; \
		acorn --ecma2024 ${file} > "${file}.acorn"; \
	done