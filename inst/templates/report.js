window.addEventListener("load", function () {
  setTimeout(function () {
    var $ = window.jQuery;
    document.querySelectorAll(".tab-content").forEach(function (content) {
      var height = 0;
      Array.prototype.forEach.call(content.children, function (pane) {
        if (!pane.classList.contains("tab-pane")) return;
        var display = pane.style.display;
        var visibility = pane.style.visibility;
        pane.style.visibility = "hidden";
        pane.style.display = "block";
        height = Math.max(height, pane.scrollHeight);
        pane.style.display = display;
        pane.style.visibility = visibility;
        if (pane.querySelector(".atlas-empty")) {
          var tab = document.querySelector('.nav-tabs a[href="#' + pane.id + '"]');
          if (tab) {
            tab.classList.add("disabled");
            tab.setAttribute("aria-disabled", "true");
            tab.title = "No " + tab.textContent.toLowerCase() + " for this table";
            tab.addEventListener("click", function (event) {
              event.preventDefault();
              event.stopPropagation();
            });
          }
        }
      });
      content.style.minHeight = height + "px";
    });
    if ($ && $.fn.dataTable) {
      $(document).on("shown.bs.tab", 'a[data-toggle="tab"]', function () {
        $.fn.dataTable.tables({ visible: true, api: true }).columns.adjust();
      });
      $.fn.dataTable.tables({ visible: true, api: true }).columns.adjust();
    }
  }, 300);
});
