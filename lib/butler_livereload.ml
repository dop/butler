let html_inject_script contents =
  Stringext.replace_all
    ~pattern:"</body>"
    ~with_:"<script src=\"/livereload.js\"></script></body>"
    contents
