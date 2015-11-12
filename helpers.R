drop_buttons <- function(url = NULL, 
                         img = NULL,
                         wid = NULL){
  obj <- " "
  for (i in 1:length(url)){
    obj <- paste0(obj, "<a href='", url[[i]],"' target='_blank'><img src = '", img[[i]], "' width='", wid[[i]], "%' alt='", url[[i]], "' itemprop='image'/></a>", collapse="")
  }
  
    obj <- paste0("\n\n<hr><div id =\"buttons\">\n<p>", obj, "</p></div>\n", collapse="")
  return(obj)
}


files2folder <- function(input=NULL, 
                         src_path=getwd(), 
                         dest_path = tempdir()){
 body_out <- files2folder_pt1(input, 
                   src_path, 
                   dest_path)
 files2folder_pt2(input, 
                  src_path, 
                  dest_path, 
                  index_body = body_out)
}

files2folder_pt1 <- function(input=NULL, 
                         src_path=getwd(), 
                         dest_path = tempdir()){
  
  dir.create(file.path(dest_path, "out"))    
  dir.create(file.path(dest_path, "out", "pdf"))
  dir.create(file.path(dest_path, "out", "img"))
  dir.create(file.path(dest_path, "src", "content"),  recursive = TRUE)
  dir.create(file.path(dest_path, "src", "structure","index","01_research"),  recursive = TRUE)
  dir.create(file.path(dest_path, "src", "structure","index","02_coding"))
  dir.create(file.path(dest_path, "src", "structure","index","03_other"))
  
  copy_folder_content(file.path(src_path, "src", "content"), file.path(dest_path, "src", "content"))
  copy_folder_content(file.path(src_path,"src", "img"), file.path(dest_path, "out", "img"))
  copy_folder_content(file.path(src_path,"src", "pdf"), file.path(dest_path, "out", "pdf"))
  tmp_con <- file.path(dest_path, "src", "content","index.txt")
 index_body <- paste(markdown::markdownToHTML(text = readLines(tmp_con), 
                                         options=c("fragment_only")), collapse="")
                                close(file(tmp_con))
  tmp_con <- file.path(dest_path, "out", "index.html")
  toFile(index_lines(content=index_body, href_favicon="input_favicon", my_buttons=drop_buttons(url = strsplit(input$b_url, " "),
                                                                                               img = strsplit(input$b_img, " "),
                                                                                               wid = strsplit(input$b_wid, " ")))[["html"]], "index.html",  file.path(dest_path, "out"))
  return(index_body)
}

files2folder_pt2 <- function(input=files2folder_pt2, 
                             src_path=getwd(), 
                             dest_path = tempdir(),
                             index_body = NULL){
  for (i in index_lines(content=index_body, href_favicon=input$favicon)[["refs"]]){
    tmp_con <- file(file.path(dest_path,"src","content", paste0(i , ".txt")))
    test <- index_lines(content=markdown::markdownToHTML(text =  
    readLines(tmp_con), options=c("fragment_only")), name = paste0(i, ".html"), href_favicon = input$favicon, my_buttons = drop_buttons(url = strsplit(input$b_url, " "),
                                                                                                                           img = strsplit(input$b_img, " "),
                                                                                                                           wid = strsplit(input$b_wid, " ")))
    close(tmp_con)
    toFile(test$html, paste0(i , ".html"),file.path(dest_path, "out"))
  }
  if (!dir.exists(file.path(dest_path,"out","img"))) {dir.create(file.path(dest_path,"out","img"))}
  toFile(built_css(col1 = input$col1, 
                   col2 = input$col2),
         "style.css",file.path(dest_path,"out"))

    #  if(input$ftp==TRUE){
    #      for (i in list.files(file.path(my_folder, "out"), include.dirs =  FALSE, recursive = TRUE)){
    #   ftpUpload(file.path(my_folder, "out", i), 
    #             paste0("ftp:/", "/", input$ftp_host, "/", i), 
    # userpwd = paste0(input$user, ":", input$ftp_pw))
    #    }
    #  }
  }

drop_folder <- function(from = my_folder, to = "webjayOutput", dirs = c("img", "pdf"), .count=0){
  if (!drop_exists(to)){drop_create(to)}
  if (any(!is.null(list.files(from, pattern = "\\.(html|css|pdf|png|jpg|ico)$")))){
    for (i in list.files(from, pattern = "\\.(html|css|pdf|png|jpg|ico)$")){
      drop_upload(file=file.path(from, i), dest = to)
      .count <- .count + 1  
    }
  }
  if (any(!is.null(dirs))){
    for (my_dir in dirs){
      if (!drop_exists(file.path(to, my_dir))){drop_create(file.path(to, my_dir))}
      
      if (any(!is.null(list.files(file.path(from, my_dir), pattern = "\\.(html|css|pdf|png|jpg|ico)$")))){
        for (i in list.files(file.path(from, my_dir), pattern = "\\.(html|css|pdf|png|jpg|ico)$")){
          drop_upload(file=file.path(from, my_dir, i), dest = file.path(to, my_dir))
          .count <- .count + 1  
        }
      }
    }  
  }
  message(paste("----------------------------\nuploaded", .count, "files to dropbox-folder", to))
}


# Wraps writeLines(x,y) and close(y) 
toFile <- function(lines, filename, folder=NULL){
  if (is.null(folder)){tmp_con = file(filename)} else {tmp_con = file(file.path(folder, filename))}
    cat(paste(lines, sep="\n", collapse = ""), file = tmp_con)
  on.exit(close(tmp_con))
}

drop_folder <- function(from, to, dirs = c("img", "pdf"), .count=0){
  if (!drop_exists(to)){drop_create(to)}
  if (any(!is.null(list.files(from, pattern = "\\.(html|css|pdf|png|jpg|ico)$")))){
    for (i in list.files(from, pattern = "\\.(html|css|pdf|png|jpg|ico)$")){
      drop_upload(file=file.path(from, i), dest = to)
      .count <- .count + 1  
    }
  }
  if (any(!is.null(dirs))){
    for (my_dir in dirs){
      if (!drop_exists(file.path(to, my_dir))){drop_create(file.path(to, my_dir))}
      
      if (any(!is.null(list.files(file.path(from, my_dir), pattern = "\\.(html|css|pdf|png|jpg|ico)$")))){
        for (i in list.files(file.path(from, my_dir), pattern = "\\.(html|css|pdf|png|jpg|ico)$")){
          drop_upload(file=file.path(from, my_dir, i), dest = file.path(to, my_dir))
          .count <- .count + 1  
        }
      }
    }  
  }
  message(paste("----------------------------\nuploaded", .count, "files to dropbox-folder", to))
}

copy_folder_content <- function(from="", to="out"){
  for (my_dir in list.dirs(from, recursive = TRUE, full.names = FALSE)){
    if (!dir.exists(paste0(to, my_dir))){dir.create(paste0(to, my_dir))}
  }
  for (i in list.files(from, recursive = TRUE, include.dirs = FALSE)){
    tmp_con1 <- paste(from,i,sep="/")
    tmp_con2 <- paste(to,i,sep="/")
    file.copy(tmp_con1, tmp_con2)
    close(file(tmp_con1))
    close(file(tmp_con2))
  }
}


index_lines <- function( 
  name = "index.html",
  css = "style.css",
  title = "Johannes Johow", 
  lang = "en",
  meta_description = "Johannes Johow",
  index_path = "src/structure",
  content = c("<h1>Test</h1>", "<p>Okay..</p>",rep("<p> <br> </p>",16)),
  submenu_sites = c("vita", "about", "newsfeeds", "impressum"),
  href_favicon = "img/favicon.ico",
  headertext="",
  my_buttons = ""){
  fragment_1_header <- c(
    "<!DOCTYPE html>",
    paste0("<html lang=\"",
           lang, 
           "\">"),
    "<head>",
    "<meta charset=\"utf-8\">",
    paste0("<meta name=\"description\" content=\"",
           meta_description, "\">"),
    "<meta name=\"viewport\" content=\"width=device-width, minimum-scale=1.0, maximum-scale=1.0\">",
    "<link href='//maxcdn.bootstrapcdn.com/font-awesome/4.2.0/css/font-awesome.min.css' rel='stylesheet'>",
    paste0("<link href=\"", css, "\" rel=\"stylesheet\" title=\"Default Stylesheet\">"),
    ifelse(!is.null(paste(href_favicon)), paste0("<link rel=\"shortcut icon\" href=\"", href_favicon, "\" type=\"image/x-icon\" />"),
           ""),
    paste0("<title>", title, "</title>"),
    "</head>",
    "<div class=\"wrapper\">",
    "<body>",
    "<ul id=\"skiplinks\">",
    "<li><a href=\"#navigation\">Home</a></li>",
    "<li><a href=\"#search\">Search</a></li>",
    "<li><a href=\"#main\">Main</a></li>",
    "</ul>",
    "<header>",paste0(headertext), 
    paste0("<hgroup lang=\"",lang, "\">"), 
    paste0("<h1><a href=\"", name, "\">joh.one</a></h1>"),
    "<h2>Johannes Johow</h2>",
    "<h2><sc>Dr. rer. nat., Dipl.-Biol.</sc><h2>",
    "</hgroup>",
    "</header>")
  
  
  structure_list <-  gsub("/structure", "", gsub(paste(index_path), "", unlist(list.dirs(index_path))))
  structure_list <- structure_list[!structure_list %in% ""]
  structure_list <- lapply(lapply(structure_list, strsplit, split="/"), unlist, recursive=FALSE)
  stopifnot(unique(unlist(lapply(structure_list, "[[", 1)))=="")
  stopifnot("index" %in% unique(unlist(lapply(structure_list, "[[", 2))))
  stopifnot(unique(unlist(lapply(lapply(structure_list[which(unlist(lapply(structure_list, "[[", 2))=="index")][-1], "[[", 3), length)))==1)
  main_sites <- unlist(lapply(strsplit(unlist(lapply(structure_list[which(unlist(lapply(structure_list, "[[", 2))=="index")][-1], "[[", 3))
                                       , split="_"), "[[", 2))
  # write top_nav 
  if (paste0("index.html" != name)){
    top_nav <- "<li><a href=\"index.html\">home</a></li>"
  } else top_nav <- "<li><span>home</span></li>"
  for (i in main_sites){
    if (paste0(i,".html") != name){
      top_nav <- c(paste(top_nav), paste0("<li><a href=\"", i, ".html\">", i, "</a></li>")) 
    } else 
      top_nav <- c(paste(top_nav), paste0("<li><span>", i, "</span></li>")) 
  }
  
  fragment_2_main_nav <- c(
    "<main>","<aside>", 
    "<h2>Main</h2>",
    "<nav id=\"navigation\">",
    "<ul>", top_nav,  
    "</ul>", 
    "</nav>")
  
  fragment_3_search  <- 
    c("<form id=\"search\" action=\"#\" method=\"post\">",
      "<fieldset>",
      "<legend>Search</legend>",
      "<p><input type=\"text\" placeholder=\"search\"></p>",
      "</fieldset>",
      "</form>", "</aside>")
  
  fragment_4_breadcrumb <- 
    c("<nav id=\"breadcrumb\">",
      "<h2>You are here:</h2>",
      ifelse(name == "index.html", 
             "<a href=\"./\"><span itemprop=\"title\">Home</span></a>",
             paste0("<a href=\"./\"><span itemprop=\"title\">",gsub(".html", "", name), "</span></a>")),
      "</nav><!-- breadcrumb ends -->")
  
  fragment_5_article <- c("<article id=\"main\">", 
                          paste(content), paste(my_buttons),
                          "</article>")
  
  
  
  
  # write sub_nav 
  if (paste0(submenu_sites[1],".html") != name){
    sub_nav <- paste0("<li><a href=\"", submenu_sites[1], ".html\">", submenu_sites[1], "</a></li>")
  } else sub_nav <- paste0("<li><span>", submenu_sites[1], "</span></li>")
  
  for (i in submenu_sites[-1]){
    if (paste0(i,".html") != name){
      sub_nav <- c(paste(sub_nav), paste0("<li><a href=\"", i, ".html\">", i, "</a></li>")) 
    } else 
      sub_nav <- c(paste(sub_nav), paste0("<li><span>", i, "</span></li>")) 
  }
  
  fragment_6_sub_nav <- c("<aside>",
                          "<h2>Submenu</h2>",
                          "<nav>",
                          "<ul id=\"sub_navigation\">",
                          sub_nav,  
                          "</ul>", 
                          "</nav>",  "</aside>","</main>", 
                          "<div class=\"push\"></div>","</div>", 
                          "<div class=\"footer\">")
  
  fragment_7_footer <- c("<footer>", "<hr />",
                         "<section class=\"vcard copyright\">",
                         "<p class=\"copy\">",
                         "<span>",
                         "&copy; <a href=\"/\">Johannes Johow</a> 2015</span>",
                         "(based on <a href=\"https://github.com/n-gin\">n-gin</a> design)", "</section>", "</footer>", "</div>","</body>","</html>")
  
  return(list("html" = c(fragment_1_header, fragment_2_main_nav,   fragment_3_search,    
                         fragment_4_breadcrumb, fragment_5_article, fragment_6_sub_nav,
                         fragment_7_footer), "refs" = unlist(c(main_sites, submenu_sites))))
}


built_css <- function(col1 = NULL, col2 = NULL, pic = "img/jayfrog.png", foot_pic =
                        "img/footfrog.png"){
  return(paste0(
    "* {
    margin: 0;
}
html, body {
height: 100%;
}

#page-wrap {
background: white;
min-width: 780px;
max-width: 1260px;
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
margin: 0 auto -65px; 
}
.footer, .push {
height: 65px; 
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
	float:none;
	width:100%;
	height:1px;
	border:none;
	margin:-1px 0;
}

buttons {
width:60%;
margin: 15px 15px 0;
padding: 0 10px 0;
text-align: right;
}

/*
Footer
*/
footer {
margin: 15px 15px 0;
padding: 0 10px 0;
font-size: .8em;
color: #00000;
text-align: left;
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
text-align: left;
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
color: #683500;
border-bottom: 1px dashed;
}
footer a:focus, footer a:hover, footer a:active {
border-bottom-style: solid;
}
footer hr {
margin: 0 0 0;
opacity: .2;
}

footer .copyright {
margin: 0 auto;
padding: 0;
display: block;
color: #000000;
font-size: .9em;
text-align: left;
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
margin-top: 10px;
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
margin-top: 75px;
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
text-align: left;
}
footer section {
width: auto;
display: block;
}
}")
)
}