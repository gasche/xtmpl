let page = [%xtmpl "page.tmpl"]

let p = page ~page_title: "The page title"
  ~contents: [Xtmpl.D "Hello world !"]
  ~value: 3
  ()
let () = print_endline (Xtmpl.string_of_xmls p)