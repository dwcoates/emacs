(defvar school-task-template "* TODO %^{Task:}
DEADLINE: %^t

  %?

Captured on %T by %(system-name)"
  "Org capture template for school tasks."
  )

(setq org-capture-templates
  (append
   `(("s" "School captures for current semester")
     ("si"
      "Inventions"
      entry
      (id "a7cc4933-6bc2-4e98-9f73-062c5fe9ea12")
      ,school-task-template
      )
     ("so"
      "Operating Systems"
      entry
      (id "3d8c6417-7dbf-4f46-9703-65aa0323c0fb")
      ,school-task-template
      )
     ("sc"
      "Chemistry"
      entry
      (id "2084f707-5cae-442f-834c-5d8e8daec6ab")
      ,school-task-template
      )
     ("sn"
      "Numanal"
      entry
      (id "4b0c2c9b-6655-4d4a-aef6-a31888826e51")
      ,school-task-template
      )
     ("sl"
      "Classics"
      entry
      (id "0c9ef8c2-8141-4a22-87c8-664ab7381b46")
      ,school-task-template
      )
     ("ss"
      "Tasks for next semester preparation"
      entry
      (id "b1f29a2c-a919-4fa1-aae0-f550ad8b44e3")
      ,school-task-template
      ))
   org-capture-templates
   )
)
