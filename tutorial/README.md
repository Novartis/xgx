xGx Interactive Tutorial Instructions
=============
How to make alterations to the website
======================

Setup
------------------
In order to connect rstudio to the shinyapps.io website for publishing, you must
first issue the command `rsconnect::setAccountInfo(name="app-name-here", token="token-here", secret="secret-here")`.
The correct command can be found here: [https://www.shinyapps.io/admin/#/tokens](https://www.shinyapps.io/admin/#/tokens) 
by logging into the shinyapps.io website with the xGx account.
(This may happen automatically in Rstudio, due to the rsconnect directory within the xgx_tutorial directory - i.e. `xgx/tutorial/xgx_tutorial_site/rsconnect/documents/index.rmd/shinyapps.io/xgx-interactive/xgx_tutorial.dcf`)

After connecting the directory to shinyapps.io, the 'publish' button in RStudio ![publish_logo](data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACgAAAAkCAYAAAD7PHgWAAACUUlEQVR42mNgGAWjYBSMVDDzPyvD9GfBDDOez2OY+fwikH4HxH8Ypj9/BRQ/CmR3MUx/ajgwjpv+PIFhxosnQEf8JwLvpZ9D59/nAFq4kkiHIePfQA/l09Zxq66wMcx4tosMxyHhl/q0jNYeHBbfAMo1MEx74QekHRhmvXABpsl4oPh0IP6AonbaSz0aOQ6YhmY8/4vmsG/AzJDOUP+fCae+ya+lgOqOAx38lGHmiwrahd6M55vQHPeTYdZz+wEqPp6HouDpz2LBxQeyA2c+L0XRAwrFGc/sgBkhl2H6iyRgdCvRMrQIJfabQAexwNXPeqIGFDuHpuYvMD1OA2cs+jvwWRpc7bznokCxx3jUz6O3A38wTHrLh1D7rI2A+n/UL1rwWTjz+UZUtS/OEwxx9PRKMZj2UBAnBtUkqBnqNhFpthGufvYLRSD/LhA/g9bdMPwaKn6YyqH9Yj1BB05/HoYUO+UE1D+ksgNfuhKw8BFD32NOJAdeoa8DIdFcj8Oyd8DawxxRGz1zJyI5PKRR4f7MDZyBIOnoCpA9mWHqU1nUhgZGWXkYGuUwXEfb6hBvUng2ASO0QI0KmoA5j4XAITTj+VpwriTURJv+bCKWqDxGy9CYg2TRB6AD2oEOdmSY+VoSWAwJQDzw1AgcXTOe38fiuO/A+lqHlg6kpNH6D9xdoHF/xAxo0RsyHAfsUD1LpU+in/1EBhitayB1LVGOuwqMdhv6505QEx6SCe5gcdRriCeeBTGs+s88CPrJ7/gZpj1VB+PZL8VHBw5GwSigEwAAO8Vb1WEAypEAAAAASUVORK5CYII=) should appear 
and be connected to the website when you open the file `xgx/tutorial/xgx_tutorial_site/index.rmd`.


Altering and adding content
-------------------
Open the R-markdown and change it as desired, or add a new R-markdown script.
Several examples can be found [here](https://rstudio.github.io/learnr/) of how to 


Publishing
--------------------------

Using RStudio, press the 'publish' button while the main file, `xgx/tutorial/xgx_tutorial_site/index.rmd`, is open.  
This takes a very long time, but it will eventually push everything to the site.
