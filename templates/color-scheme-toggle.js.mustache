(function(f){
if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", f);
} else {
  f();
}})(function() {
  const btn = document.getElementById('color-scheme-btn')
  btn.innerHTML = localStorage.getItem('color-scheme') || 'auto'
  btn.addEventListener("click", e => {
    const colorSchemes =
      window.matchMedia('(prefers-color-scheme: dark)').matches
      ? [ 'auto', 'light', 'dark' ]
      : [ 'auto', 'dark', 'light' ]
    let scheme = localStorage.getItem('color-scheme') || 'auto'
    scheme = colorSchemes[(colorSchemes.indexOf(scheme) + 1) % colorSchemes.length]
    localStorage.setItem("color-scheme", scheme);
    btn.innerHTML = scheme
    if (scheme === 'auto') {
      scheme = window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light'
    }
    document.documentElement.setAttribute('color-scheme', scheme)
  })
});
