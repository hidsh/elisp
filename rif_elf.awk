#!/usr/bin/awk
#
# rif_elf.el
#
# command line:
#   readelf --debug-dump=macro <elf-file> | gawk -f /path/to/rif_elf.el

function get_val_exlclude_parentheses(s) {
	beg = match($9, /\(/);

	return substr($9, beg+1, match($9, /\)/) - 1 - beg);
}

/DW_MACINFO_define/ {
# 	no_val = "__rif_no_value_rif__"
	no_val = "1"

	gsub(/\([:alnum:]+\)/, get_val_exlclude_parentheses($9), $9);
	if($8 !~ /(\()|(\))/ && $9 !~ /(\()|(\))|(\\)|(\")|(')|([\[\]])/) {
		gsub(/[;]/, "", $9);
# 		gsub(/^0[x]0+$/, "0", $9);
		gsub(/^0[x][0-9a-zA-Z]+/, sprintf("(string-to-number \"%s\" 16)", substr($9, 3)), $9);
		gsub(/^[:blank:]*$/, no_val, $9);
		printf("  (%s . %s)\n", $8, $9);
	}
}

# rif_elf.el ends here
