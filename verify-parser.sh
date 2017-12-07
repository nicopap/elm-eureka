cargo build --example display_parser
for file in $(find examples/friendsketch -name '*.elm') ; do
	if ! ./target/debug/examples/display_parser $file ; then
		echo -e "\e[7m$(tput setaf 5)$file$(tput sgr0)"
	fi
done | less
