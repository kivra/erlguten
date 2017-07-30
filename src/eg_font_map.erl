-module(eg_font_map).
-export([handler/1,all_fonts/0]).
handler("Courier-Bold")-> eg_font_1;
handler("Courier-BoldOblique")-> eg_font_2;
handler("Courier")-> eg_font_3;
handler("Courier-Oblique")-> eg_font_4;
handler("Helvetica-Bold")-> eg_font_5;
handler("Helvetica-BoldOblique")-> eg_font_6;
handler("Helvetica")-> eg_font_7;
handler("Helvetica-Oblique")-> eg_font_8;
handler("Symbol")-> eg_font_9;
handler("Times-Bold")-> eg_font_10;
handler("Times-BoldItalic")-> eg_font_11;
handler("Times-Roman")-> eg_font_12;
handler("Times-Italic")-> eg_font_13;
handler("ZapfDingbats")-> eg_font_14;
handler("Victorias-Secret")-> eg_font_15;
handler("OCR-A-Digits")-> eg_font_16;
handler("OCR-B-Digits")-> eg_font_17;
handler("Free3of9Extended")-> eg_font_18;
handler(_) -> undefined.
all_fonts() -> ["Courier-Bold","Courier-BoldOblique","Courier","Courier-Oblique",
 "Helvetica-Bold","Helvetica-BoldOblique","Helvetica","Helvetica-Oblique",
 "Symbol","Times-Bold","Times-BoldItalic","Times-Roman","Times-Italic",
 "ZapfDingbats","Victorias-Secret","OCR-A-Digits","OCR-B-Digits",
 "Free3of9Extended"].
