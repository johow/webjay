source("built_css.R")

drop_buttons <- function(url = NULL, 
                         img = NULL,
                         wid = NULL){
  obj <- " "

  
  for (i in 1:length(url)){
    obj <- paste0(obj, "<a href='", url[[i]],"' target='_blank'><img src = '", img[[i]], "' width='", wid[[i]], "%' alt='", url[[i]], "' itemprop='image'/></a>", collapse="")
  }
  
    obj <- paste("\n\n<p><br/>", obj, "<br/></p><hr/><p></p>\n", collapse="", sep="")
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
  toFile(index_lines(content=index_body, href_favicon="input_favicon", 
                     my_buttons=drop_buttons(url = strsplit(input$b_url, " "),
                                                    img = strsplit(input$b_img, " "),
                                                    wid = strsplit(input$b_wid, " ")))[["html"]],
                     "index.html",  file.path(dest_path, "out"))
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
  submenu_sites = c("about"),
  href_favicon = "img/favicon.ico",
  headertext="",
  my_buttons = "",
  my_site = "joh.one"){
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
 #   "<li><a href=\"#search\">Search</a></li>",
    "<li><a href=\"#main\">Main</a></li>",
    "</ul>",
    "<header>",paste0(headertext), 
    paste0("<hgroup lang=\"",lang, "\">"), 
    paste0("<h1><a href=\"index.html\">", my_site, "</a></h1>"),
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
  # write sub_nav 
  if (paste0(submenu_sites[1],".html") != name){
    sub_nav <- paste0("<p><a href=\"", submenu_sites[1], ".html\">", submenu_sites[1], "</a></p>")
  } else sub_nav <- paste0("<p><span>", submenu_sites[1], "</span></p>")
  
  for (i in submenu_sites[-1]){
    if (paste0(i,".html") != name){
      sub_nav <- c(paste(sub_nav), paste0("<p><a href=\"", i, ".html\">", i, "</a></p>")) 
    } else 
      sub_nav <- c(paste(sub_nav), paste0("<p><span>", i, "</span></p>")) 
  }
  
 
  fragment_2_main_nav <- c(
    "<main>","<aside>", 
    "<h2>My Projects</h2>",
    "<nav id=\"navigation\">",
    "<ul>", top_nav,  
    "</ul>", 
    "</nav>", 
    sub_nav, 
    "</aside>")
  
#   fragment_3_search  <- 
#     c("<form id=\"search\" action=\"#\" method=\"post\">",
#       "<fieldset>",
#       "<legend>Search</legend>",
#       "<p><input type=\"text\" placeholder=\"search\"></p>",
#       "</fieldset>",
#       "</form>")
  
  fragment_4_breadcrumb <- 
    c("<nav id=\"breadcrumb\">",
      "<h2>You are here:</h2>",
      ifelse(name == "index.html", 
             "<a href=\"./\"><span itemprop=\"title\">Home</span></a>",
             paste0("<a href=\"./\"><span itemprop=\"title\">",gsub(".html", "", name), "</span></a>")),
      "</nav><!-- breadcrumb ends -->")
  
  fragment_5_article <- c("<article id=\"main\">", 
                          paste(content),"<hr/>", paste(my_buttons),
                          "</article>")
  
  
  
  
 
  
  fragment_7_footer <- c("</main></div><div class=\"footer\">", "<footer>", "<hr />",
                         "<section class=\"vcard copyright\">",
                         "<p class=\"copy\">",
                         "<span>",
                         "&copy; <a href=\"/\">Johannes Johow</a> 2015</span>",
                         "</section>", "</footer>", "</div>","</body>","</html>")
  
  return(list("html" = c(fragment_1_header, 
                         fragment_2_main_nav,  # fragment_3_search,   
                         fragment_4_breadcrumb, 
                         fragment_5_article, # fragment_6_sub_nav,
                         fragment_7_footer), "refs" = unlist(c(main_sites, submenu_sites))))
}

