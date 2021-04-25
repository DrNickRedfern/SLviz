about_panel <- tabPanel(
  "About",
  
  titlePanel("SLviz"),
  
  p("Version: 0.1.0 (April 2021)"),
  p("Access the code for this app on Github:"),
  p(tags$i(
    class = "fa fa-github-square", 
    style = "vertical-align: middle; font-size: 24px; padding-right: 12px; padding-bottom: 5px;"),
    tags$a(href = "https://github.com/DrNickRedfern/SLviz", "DrNickRedfern/SLviz")),
  tags$hr(style = "border-color: purple;"),
  p("Author: Nick Redfern"),
  p(tags$i(
    class = "fab fa-orcid", 
    style = "vertical-align: middle; font-size: 24px; padding-right: 12px; padding-bottom: 5px;"), tags$a(href = "https://orcid.org/0000-0002-7821-2404","0000-0002-7821-2404")),
  tags$i(class = "fab fa-researchgate", 
         style = "vertical-align: top; font-size: 24px; padding-right: 12px; padding-bottom: 15px;"), 
  tags$a(href = "https://www.researchgate.net/profile/Nick_Redfern", "ResearchGate/NickRedfern"),
  p(tags$i(
    class = "fab fa-wordpress", 
    style = "vertical-align: middle; font-size: 24px; padding-right: 12px; padding-bottom: 5px;"), tags$a(href = "https://computationalfilmanalysis.wordpress.com", "https://computationalfilmanalysis.wordpress.com")),
  tags$hr(style = "border-color: purple;"),
  tags$h4("License"),
  p("Copyright (C) 2021  Nick Redfern"),
  p("This program is free software: you can redistribute it and/or modify it under the terms of the GNU General 
     Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) 
     any later version."),
  p("This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
     without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
     See the GNU General Public License for more details."),
  p(tags$a(href = "https://github.com/DrNickRedfern/SLviz/blob/main/LICENSE", "GNU General Public License Version 3 (GPLv3)."))

)
