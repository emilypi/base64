#include "../include/tables.h"

#include "../include/table_dec_32bit_std.h"
#include "../include/table_dec_32bit_url.h"
#include "../include/table_enc_12bit_std.h"
#include "../include/table_enc_12bit_url.h"

const uint8_t
base64_table_enc_6bit_std[] =
	"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	"abcdefghijklmnopqrstuvwxyz"
	"0123456789"
	"+/";

const uint8_t
base64_table_enc_6bit_url[] =
	"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	"abcdefghijklmnopqrstuvwxyz"
	"0123456789"
	"-_";
