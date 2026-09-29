---
title: "{{title}}"
author: "{{author}}"
date: "{{timestamp}}"
output:
  html_document:
    toc: true
    toc_float: true
    theme: flatly
    self_contained: true
---

<style>
{{{style}}}
</style>

<script>
{{{report_script}}}
</script>

{{{analytics}}}

{{#has_logo}}<img src="{{logo}}" alt="Report logo" class="atlas-logo">{{/has_logo}}

{{abstract}}

{{{body}}}
