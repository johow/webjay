built_css <- function(col1 = NULL, col2 = NULL, pic = "img/jayfrog.png", footer_px =
                        28){
  return(paste0(
    "* {
    margin: 0;
}
html, body {
height: 99%;
}

html {
min-width: 100%;
}

#page-wrap {
background: white;
min-width: 800px;
max-width: 1600px;
margin: 10px auto;
}
#page-wrap #inside {
margin: 10px 10px 0px 10px;
padding-top: 10px;
padding-bottom: 10px;
}
.wrapper {
min-height: 100%;
height: auto !important;
height: 100%;
margin: 0 auto -", footer_px, "px; 
}
.footer, .push {
height: ", footer_px, "px; 
} 
html, body, div, span, applet, object, iframe,
h1, h2, h3, h4, h5, h6, p, blockquote, pre,
a, abbr, acronym, address, big, cite, code,
del, dfn, em, img, ins, kbd, q, s, samp,
small, strike, strong, sub, sup, tt, var,
b, u, i, center,
dl, dt, dd, ol, ul, li,
fieldset, form, label, legend,
table, caption, tbody, tfoot, thead, tr, th, td,
article, aside, canvas, details, embed,
figure, figcaption, footer, header, hgroup,
menu, nav, output, ruby, section, summary,
time, mark, audio, video {
margin: 0;
padding: 0;
border: 0;
font-size: 100%;
font: inherit;
vertical-align: baseline;
}

body {
line-height: 1;
}

ol, ul {
list-style: none;
}

table {
border-collapse: collapse;
border-spacing: 0;
}

caption, th, td {
text-align: left;
font-weight: normal;
vertical-align: middle;
}

q, blockquote {
quotes: none;
}
q:before, q:after, blockquote:before, blockquote:after {
content: \"\";
content: none;
}

a img {
border: none;
}

article, aside, details, figcaption, figure, footer, header, hgroup, menu, nav, section, summary {
display: block;
vertical-align: baseline;
}

header h1 {
font-weight: bolder;
}
header h2 {
font-weight: bold;
}

/*
Styles starts
*/
html {
overflow-y: scroll;
/* Horizontale Scrollbalken erzwingen */
}

body {
margin: 0 auto;
color: #555;
font: normal 1em/150% Geneva, Helvetica, sans-serif, Verdana;
letter-spacing: 1px;
max-width: 1100px;
position: relative;
}

a {
text-decoration: none;
color: #0e8a66;
}
a:focus, a:hover, a:active {
color: #ff7e00;
}

strong {
font-weight: bold;
}

/*
Wenn ein Element ein title-Attribut hat
*/
[title] {
cursor: help;
}

a [title],
a[title] {
cursor: pointer;
}

table {
margin: 15px 0;
border: 1px solid #355e64;
border-collapse: collapse;
}
table th,
table td {
width: 150px;
height: 50px;
font-size: .8em;
text-align: center;
font-weight: bold;
border: 1px solid;
cursor: default;
}
table th {
height: 30px;
background: #355e64;
color: #FFF;
border: 1px solid #355e64;
border-right: 1px solid #FFF;
}
table th:nth-child(5) {
border-right: 1px solid #355e64;
}
table tfoot td {
height: 25px;
font-size: .8em;
}

/*
Skiplinks
*/
#skiplinks {
position: absolute;
top: -1000px;
left: -1000px;
}

#skiplinks a:focus,
#skiplinks a:active {
position: absolute;
top: 1005px;
left: 1015px;
z-index: 5;
white-space: nowrap;
min-width: 250px;
display: block;
padding: 5px 15px;
color: #F2F2F3;
font-weight: bold;
background: #19363E;
border: 1px solid #c1e7f2;
border-width: 1px 2px;
border-radius: 10px;
outline: 0;
}

/*
Flexibles Design und die Leistung von Medienabfragen nutzen
http://googlewebmastercentral-de.blogspot.de/2012/06/flexibles-design-und-die-leistung-von.html?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed:+blogspot/vIRG+%28Webmaster-Zentrale+Blog%29
*/
/* screen */
@media screen, projection {
/*
Site-Header
*/
header {
margin-bottom: 0px;
padding: 0 20px 0px 15px;
color: #FFF;
position: relative;
border-bottom: 5px solid #9eae5d;
background-color: #577533;
background-size: contain;
background-image: -webkit-linear-gradient(top, ", col2, " 0%, ",  col1, " 100%);
background-image:  url(\"",pic,"\"), linear-gradient(to bottom,", col2, " 0%, ", col1, " 100%);
background-repeat: no-repeat;
background-position: 99% 99%;
box-shadow: 0 5px 10px #555555;
}
header sc {
font-size: 66%;
font-variant: small-caps;  
}
header h1 {
padding: 60px 0 10px 0;
font-size: 2em;
}
header h1 a {
color: #354103;
}
header h1 a:focus, header h1 a:hover, header h1 a:active {
color: #de4411;
}
header h2 {
color: #000000;
padding:0 0 0 0;
font-weight: bolder;  
font-size: 1.4em;
}

main {
margin: 20px 0;
padding: 0 15px;
overflow: hidden;
}

/*
Sidebar
*/
aside {
width: 23%;
float: left;
}
aside p {
padding: 5px;
text-indent: -4px;
}
aside p span {
color: #82B2BC;
}

aside form {
margin: 35px 0 40px;
}
aside form legend {
margin-bottom: 10px;
display: block;
width: 100%;
}
aside form p {
margin: 0;
padding: 10px 0 0;
}
aside form p input {
padding: 4px 4px;
}
aside form p input[type=\"text\"] {
width: 95%;
background: white;
border-color: #779EC4;
border-style: outset;
border-radius: 5px;
color: #999;
}
aside form p input[type=\"text\"]:focus {
color: #333;
background: white;
border-color: #AAA;
}
aside h2, aside h3, aside h4, aside fieldset legend {
margin-bottom: 10px;
font-size: .7em;
font-weight: bold;
line-height: 130%;
color: #BAC5C7;
border-bottom: 1px solid;
cursor: default;
}

aside #network_tools > div {
display: inline;
}
aside nav li {
color: #b0b6b7;
padding: 0;
}
aside nav a, aside nav span {
padding: 5px 0;
display: block;
border-bottom: 1px solid;
}
aside nav span {
color: #82B2BC;
}
aside img {
vertical-align: basement;
}

/*
Article
*/
#breadcrumb {
width: 73%;
float: right;
margin-bottom: 10px;
font-size: .7em;
font-weight: bold;
line-height: 130%;
color: #BAC5C7;
border-bottom: 1px solid;
cursor: default;
}
#breadcrumb h2, #breadcrumb div {
margin: 0;
padding: 0;
display: inline;
}

article {
float: right;
width: 73%;
}
article h1, article h2, article h3, article h4, article h5, article h6 {
padding: 25px 0;
font-size: 1.5em;
font-weight: bold;
}
article h1 {
padding: 25px 0 0;
}
article p {
margin: 25px 0;
}
article ul {
margin-left: 15px;
line-height: 170%;
list-style: disc;
}
article ul ul {
margin-left: 25px;
list-style: square;
}
article img {
margin: 5px 5px 3px 15px;
float: right;
border: 1px solid #FFFFFF;
border-radius: 10px;
}
article blockquote {
padding: 15px 10px 5px 15px;
font-size: 1.1em;
font-weight: bold;
line-height: 140%;
color: #B6B6B6;
background-color: #fdecbe;
border: 1px solid;
border-radius: 10px;
background-size: 100%;
}
article blockquote cite {
margin-right: 15px;
display: block;
text-align: right;
font-size: .7em;
}
article blockquote cite a {
border-bottom: 1px dashed;
}
article blockquote cite a:focus, article blockquote cite a:hover, article blockquote cite a:active {
border-bottom-style: solid;
}

article hr {
background:none;
clear:both;
width:100%;
height:1px;
border:none;
margin:-1px 0;
}

/*
Footer
*/
footer {
margin: 15px 15px 0;
padding: 0 10px 0;
font-size: .8em;
color: white;
text-align: middle;
border-top: 2px solid #859b83;
background-color: #577538;
background-size: contain;
background-image: -webkit-linear-gradient(top, ", col2, " 0%, ",  col1, " 100%);
background-repeat: no-repeat;
background-position: 99% 99%;
border-top-right-radius: 20px;
border-top-left-radius: 20px;
}
footer section {
display: inline-block;
vertical-align: top;
margin: 0;
padding: 0 0;
width: 1;
text-align: middle;
}
footer section h1, footer section h2, footer section h3, footer section h4, footer section h5 {
font-weight: bold;
padding: 5px 0 px;
}
footer section h3 {
padding-top: 0px;
}
footer section.about_us p {
padding-right: 0px;
}
footer section .org {
font-weight: bold;
}
footer a {
color: #000;
border-bottom: 1px dashed;
}
footer a:focus, footer a:hover, footer a:active {
border-bottom-style: solid;
color: white;
}
footer hr {
margin: 0 0 0;
opacity: .2;
}

footer .copyright {
margin: 0 auto;
padding: 0;
display: block;
color: white;
font-size: .9em;
text-align: middle;
cursor: default;
}
footer .copyright span {display:block;}
}
/* footer ends */
/* screen, projection ends */
/*
Drucklayout
*/
@media print {
header h2,
aside,
#breadcrumb,
.about_us,
.partner_sites,
#footer_menu,
.copyright,
hr {
display: none !important;
}

h1 {
margin-bottom: 35px;
font-size: 1.4em;
font-weight: bold;
}

h2, h3, h4, h5, h6, p, table, ul {
margin: 25px 0;
}

h2, h3, h4, h5, h6, strong {
font-weight: bold;
}

ul {
margin-left: 20px;
}

a {
color: #000;
text-decoration: none;
}

address {
padding: 2px 0;
}

.contact_to,
.email {
padding-top: 10px;
}
}
/* print ends */
/*
vmtl. Tablet
*/
@media screen and (max-width: 800px) {
footer {
margin-left: 0;
margin-right: 0;
}
footer section {
width: 45%;
}
}
/* Tablet ends */
/*
Smartphone
*/
@media screen and (max-width: 600px) {
html {
min-width: 120px;
}
body {
font-size: 20px;
}

header {
margin-bottom: 40px;
padding-right: 5px;
font-size: 85%;
}
header:before {
padding: 0 5px;
content: \"Mobile Version\";
position: absolute;
top: 5px;
left: 10px;
font-size: .8em;
color: #707C7D;
border: 1px solid;
line-height: 140%;
cursor: default;
border-radius: 5px;
opacity: 0.8;
}

aside {
width: auto;
float: none;
}
aside form {
margin-bottom: 30px;
}
aside form legend {
display: none;
}
aside form p input[type=\"text\"] {
padding: 9px 5px;
background: none;
font-size: 1.1em;
color: #BBCEE1;
}
aside form p input[type=\"text\"]:focus {
background: none;
border-color: #265258;
color: #333;
box-shadow: #949494 0 0 10px;
}

aside:last-child {
padding-top: 75px;
}

#breadcrumb {
margin: 10px 0 30px;
padding: 0;
width: auto;
float: none;
}

article {
margin-left: 0;
padding: 0;
width: auto;
float: none;
}
article img {
margin: 0;
float: none;
}

footer {
text-align: middle;
}
footer section {
width: auto;
display: block;
text-color: white;
}
}")
)
}