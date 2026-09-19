/* =============================================================================
   scripts/site.js: the one JavaScript file for the whole site.

   Loaded on every page by _quarto.yml (include-after-body), immediately after
   the Fancybox library. It does four jobs that used to be scattered across a
   dozen places:

     1. Fancybox init:   lightbox popups. Was an inline <script> in _quarto.yml.
     2. Hero slideshows: the cross-fading image band at the top of a page. Was
                         copy-pasted into seven pages, each copy slightly
                         different: photography, contact, and the project pages
                         now called songs, lobster-model, kelp-forest-monitoring,
                         pmrg and nicothoid-copepod.
     3. Masonry shuffle: randomizes gallery order. Was copy-pasted identically
                         into the four photography galleries (marine,
                         terrestrial, portraits, microscopy).
     4. Footnote band:   collapses the endnote card at the foot of a project
                         page behind one clickable bar. Written here first;
                         there was never a page-level version of it.

   HOW A PAGE OPTS IN. No page needs its own JavaScript. A page gets a behavior
   by using the right class, and everything here stays quiet when the class is
   absent:

     Slideshow   class="hero-slideshow" on a container of <a> slides. Optional
                 data-delay="3000", milliseconds per slide, default 3000. Also
                 used mid-page on nicothoid-copepod, where figure panels cycle:
                 same class, plus .figure-slideshow for the sizing rules in
                 styles.css section 31.

     Footnotes   Nothing at all. Any page whose render produces a
                 <section class="footnotes"> gets the band; a page with no
                 notes is left alone. Styling is styles.css section 15.

     Shuffle     class="masonry" AND data-randomize="always" or "daily".
                 scripts/render_gallery.R emits exactly that, so the four
                 generated galleries need nothing extra. The art page's
                 .masonry deliberately has no data-randomize, so its order
                 stays fixed.

   WHY ONE FILE. A behavior change, say pausing slideshows for people who ask
   their operating system to reduce motion, happens once here rather than in
   seven to eleven copies that have already drifted apart.

   WHERE THE STYLING LIVES. This file adds and removes classes and sets almost
   no inline styles. What those classes look like is in styles.css: section 17
   for hero slideshows, 31 for in-text figure slideshows, 15 for the footnote
   band. If something looks wrong rather than behaves wrong, the fix is there,
   not here.
   ============================================================================= */

(function () {                       /* Wrap everything in a function that runs
                                        immediately (an "IIFE"). Variables inside
                                        stay private and can't collide with
                                        Quarto's, Bootstrap's, or Leaflet's. */
  'use strict';                      /* Strict mode: JS throws on sloppy mistakes
                                        (undeclared variables, etc.) instead of
                                        silently doing something odd. */


  /* ---------------------------------------------------------------------------
     1) FANCYBOX: lightbox popups for photos and videos
     Moved verbatim from _quarto.yml. Any <a data-fancybox="…"> on the page
     opens its href in a lightbox; anchors sharing the same data-fancybox value
     become one swipeable album.
  --------------------------------------------------------------------------- */
  function initFancybox() {
    if (window.Fancybox) {           /* Only run if the CDN library actually
                                        loaded (offline / CDN outage safety). */
      Fancybox.bind('[data-fancybox]', {});
                                     /* Attach Fancybox to every element that has
                                        a data-fancybox attribute. {} = default
                                        options. */
    }
  }


  /* ---------------------------------------------------------------------------
     2) HERO SLIDESHOWS: cross-fade between <a> slides inside .hero-slideshow
     The CSS (styles.css section 17) stacks all slides on top of each other at
     opacity 0 and shows whichever one has class "active". All this JS does is
     move the "active" class from slide to slide on a timer.
  --------------------------------------------------------------------------- */
  function initSlideshow(el) {       /* el = one .hero-slideshow element. */
    var slides = Array.prototype.slice.call(
                   el.querySelectorAll(':scope > a, :scope > p > a'));
                                     /* Collect the slide anchors as a real
                                        array. ":scope > a" means "anchors that
                                        are immediate children", which keeps a
                                        nested link (none today) from being
                                        mistaken for a slide.

                                        The ":scope > p > a" half is not
                                        optional. Pandoc parses the inside of a
                                        raw HTML block as Markdown, and a run of
                                        <a> elements separated only by newlines
                                        reads to it as one paragraph, so it
                                        emits <div class="hero-slideshow"><p>
                                        wrapping every slide. That is true of
                                        ALL eleven slideshows on the site, not
                                        a quirk of one page. With only the first
                                        half of this selector, every slideshow
                                        matched zero slides and stopped at the
                                        length check below.

                                        Why it went unnoticed: the page-level
                                        copies this file replaced used a plain
                                        'a' selector, which reaches through the
                                        wrapper. They were masking the fault
                                        until they were deleted.

                                        querySelectorAll returns document order
                                        regardless of which half of the
                                        selector matched, so slide order is
                                        preserved either way. */
    if (slides.length === 0) return; /* Nothing to show; bail. */

    var i = 0;                       /* Index of the slide currently visible. */

    function show(n) {               /* Make slide n the only "active" one. */
      slides.forEach(function (s, idx) {
        s.classList.toggle('active', idx === n);
                                     /* toggle(cls, force): add the class when
                                        force is true, remove it when false. */
      });
    }

    show(0);                         /* Always start on the first slide. The
                                        HTML also hardcodes class="active" on
                                        slide 1; this makes that optional. */

    if (slides.length < 2) return;   /* One image means nothing to rotate, which
                                        is the case on lobster-model and
                                        coral-regeneration. Before this file,
                                        two such pages still ran a timer that
                                        toggled a single slide on and off.
                                        Wasted work, now skipped. */
    var reduce = window.matchMedia &&
                 window.matchMedia('(prefers-reduced-motion: reduce)').matches;
                                     /* Honor the OS/browser accessibility
                                        setting "reduce motion". If on, leave
                                        slide 1 up and never auto-advance.
                                        Only 4 of the 7 old copies did this;
                                        now every slideshow does. */
    if (reduce) return;

    var delay = parseInt(el.getAttribute('data-delay'), 10) || 3000;
                                     /* Read the per-slideshow speed from an
                                        optional data-delay="…" attribute
                                        (milliseconds). parseInt returns NaN if
                                        the attribute is missing, and NaN is
                                        falsy, so "|| 3000" supplies the
                                        default. */

    function next() {                /* Advance one slide, wrapping to 0. */
      i = (i + 1) % slides.length;
      show(i);
    }

    var timer = setInterval(next, delay);
                                     /* Start auto-rotation. setInterval returns
                                        an id we keep so we can stop it. */

    el.addEventListener('mouseenter', function () {
      clearInterval(timer);          /* Pause while the pointer is over the
                                        slideshow so a visitor can click a slide
                                        (which opens the Fancybox lightbox). */
    });
    el.addEventListener('mouseleave', function () {
      timer = setInterval(next, delay);
                                     /* Resume when the pointer leaves. */
    });
  }

  function initSlideshows() {        /* Find every slideshow on the page. */
    document.querySelectorAll('.hero-slideshow').forEach(initSlideshow);
  }


  /* ---------------------------------------------------------------------------
     3) MASONRY SHUFFLE: randomize gallery order on page load
     Galleries are built by render_gallery.R in alphabetical filename order.
     Shuffling in the browser gives every visit a fresh arrangement without
     re-rendering the site.

     Two modes, read from the gallery's data-randomize attribute:
       "always": a different order every page load (Math.random).
       "daily":  the same order for everyone all day, new order tomorrow.
                  Achieved by seeding a tiny pseudo-random generator with
                  today's date.
     NOTE: the old page-level copies hard-coded MODE = 'always' and ignored the
     attribute that render_gallery.R was emitting. This version actually reads it.
  --------------------------------------------------------------------------- */
  function mulberry32(a) {           /* A small, fast seeded random-number
                                        generator. Given the same seed "a", it
                                        produces the same sequence every time,
                                        that's what makes "daily" mode stable.
                                        The constants are the published
                                        mulberry32 algorithm; treat as a
                                        black box. */
    return function () {
      var t = (a += 0x6D2B79F5);
      t = Math.imul(t ^ (t >>> 15), t | 1);
      t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
      return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
                                     /* Returns a number in [0, 1), like
                                        Math.random(). */
    };
  }

  function dailySeed() {             /* Turn today's date into an integer seed. */
    var day = new Date().toISOString().slice(0, 10);
                                     /* "YYYY-MM-DD" in UTC. */
    var seed = 2166136261;           /* FNV-1a hash starting value. */
    for (var k = 0; k < day.length; k++) {
      seed ^= day.charCodeAt(k);     /* Mix in each character code… */
      seed = Math.imul(seed, 16777619);
                                     /* …then multiply by the FNV prime. */
    }
    return seed >>> 0;               /* Force to an unsigned 32-bit integer. */
  }

  function shuffleNodes(container, mode) {
                                     /* Reorder the direct children of
                                        container in place. */
    var items = Array.prototype.slice.call(container.children);
                                     /* Live HTMLCollection → static array. */
    if (items.length < 2) return;    /* Nothing to shuffle. */

    var rand = (mode === 'daily') ? mulberry32(dailySeed()): Math.random;
                                     /* Pick the random source based on mode. */

    for (var i = items.length - 1; i > 0; i--) {
                                     /* Fisher–Yates shuffle: walk from the end,
                                        swapping each item with a random earlier
                                        one. Produces a uniformly random order. */
      var j = Math.floor(rand() * (i + 1));
      var tmp = items[i]; items[i] = items[j]; items[j] = tmp;
    }

    var frag = document.createDocumentFragment();
                                     /* An off-screen container. Appending the
                                        nodes here first, then once to the page,
                                        means the browser re-lays-out once
                                        instead of once per image. */
    items.forEach(function (n) { frag.appendChild(n); });
    container.appendChild(frag);     /* Moving nodes out of and back into the
                                        container in the new order = shuffled. */
  }

  function initShuffle() {           /* Find every gallery that asked for it. */
    document.querySelectorAll('.masonry[data-randomize]').forEach(function (g) {
      var mode = g.getAttribute('data-randomize');
      if (mode === 'always' || mode === 'daily') shuffleNodes(g, mode);
                                     /* Any other value (e.g. "off") = leave the
                                        gallery in its rendered order. */
    });
  }


  /* ---------------------------------------------------------------------------
     4) FOOTNOTE BAND: collapse the endnote card behind its own heading

     Quarto renders collected endnotes as roughly:

         <div id="quarto-appendix" class="default">
           <section class="footnotes footnotes-end-of-document">
             <h2 class="quarto-appendix-heading">Footnotes</h2>
             <hr>
             <ol> ... </ol>
           </section>
         </div>

     On the project pages that is five long citations sitting open at the foot
     of every page. This turns the h2 into a button, moves the list into a
     panel under it, and starts the panel closed. The card itself keeps the
     background, border and coral top edge set in styles.css section 15; only
     the padding moves, onto the button and the panel, so the closed state is a
     clean band with nothing hanging below it.
  --------------------------------------------------------------------------- */
  function initFootnoteBand() {
    var sec = document.querySelector('section.footnotes, div.footnotes');
                                     /* Both forms, matching the two selectors
                                        used throughout section 15 of the
                                        stylesheet: pandoc emits a <section> in
                                        HTML5 mode and a <div> otherwise. */
    if (!sec || sec.dataset.fnReady) return;
                                     /* No notes on this page, or the band has
                                        already been built (guards against a
                                        double init if this ever runs twice). */
    if (!sec.querySelector('ol')) return;
                                     /* A footnote section with no list is
                                        nothing to collapse. */
    sec.dataset.fnReady = '1';

    var heading = sec.querySelector('h2, h3, .quarto-appendix-heading');
    var label = heading ? heading.textContent.trim(): 'Footnotes';
                                     /* Reuse whatever pandoc called it rather
                                        than hardcoding the word, so a
                                        translated or renamed heading still
                                        reads correctly on the band. */
    if (heading) heading.remove();

    var rule = sec.querySelector(':scope > hr');
    if (rule) rule.remove();         /* Pandoc's divider under the heading. The
                                        button carries its own bottom border
                                        when open, so this would double up. */

    var inner = document.createElement('div');
    inner.className = 'fn-inner';
    while (sec.firstChild) inner.appendChild(sec.firstChild);
                                     /* Everything still in the section, which
                                        by now is the <ol> and whitespace,
                                        becomes the panel contents. */

    var body = document.createElement('div');
    body.className = 'fn-body';
    body.id = 'fn-body-panel';
    body.appendChild(inner);

    var btn = document.createElement('button');
    btn.type = 'button';             /* Without this a button inside a form
                                        defaults to type="submit". No forms
                                        here, but it costs nothing. */
    btn.className = 'fn-toggle';
    btn.setAttribute('aria-expanded', 'false');
    btn.setAttribute('aria-controls', 'fn-body-panel');
                                     /* The two attributes a screen reader needs
                                        to announce this as a disclosure and to
                                        report whether it is open. */
    btn.innerHTML = '<span class="fn-label"></span>' +
                    '<span class="fn-count"></span>' +
                    '<span class="fn-chevron" aria-hidden="true"></span>';
    btn.querySelector('.fn-label').textContent = label;
                                     /* textContent, not innerHTML, so a stray
                                        angle bracket in a heading can never
                                        become markup. */

    var n = inner.querySelectorAll('ol > li').length;
    btn.querySelector('.fn-count').textContent = n ? String(n): '';
                                     /* The note count, so a reader can see
                                        whether opening the band is worth it.
                                        Empty string when somehow zero, and the
                                        stylesheet hides an empty pill. */

    sec.classList.add('fn-collapse');
    sec.appendChild(btn);
    sec.appendChild(body);

    function setOpen(open) {
      sec.classList.toggle('fn-open', open);
      btn.setAttribute('aria-expanded', open ? 'true': 'false');
                                     /* Class drives the CSS, attribute drives
                                        assistive technology. Both, always. */
    }

    btn.addEventListener('click', function () {
      setOpen(!sec.classList.contains('fn-open'));
    });

    /* A superscript marker in the body text links to a note that is now inside
       a collapsed panel. Left alone, the browser would jump to something with
       no height and the reader would land on the footer. Intercept those links
       only: open the band, wait out the expand transition, then scroll. */
    document.addEventListener('click', function (e) {
      var a = e.target.closest ? e.target.closest('a'): null;
      if (!a) return;

      var href = a.getAttribute('href') || '';
      if (href.charAt(0) !== '#') return;

      var target = document.getElementById(decodeURIComponent(href.slice(1)));
      if (!target || !sec.contains(target)) return;
                                     /* Only links pointing INTO the panel. The
                                        return arrows inside each note point
                                        back out to #fnref…, which fails this
                                        test and keeps its normal behaviour. */
      e.preventDefault();
      setOpen(true);
      window.setTimeout(function () {
        target.scrollIntoView({ behavior: 'smooth', block: 'center' });
        history.replaceState(null, '', href);
                                     /* replaceState rather than pushState: the
                                        address bar shows the note, but the back
                                        button still goes to the previous page
                                        instead of undoing a scroll. */
      }, 300);                       /* Slightly longer than the 0.28s panel
                                        transition in the stylesheet. Change
                                        both together or neither. */
    });

    if (location.hash && sec.querySelector(location.hash)) setOpen(true);
                                     /* Someone arriving on a shared link that
                                        ends in #fn3 should land with the band
                                        already open. */
  }


  /* ---------------------------------------------------------------------------
     5) BOOT: run everything once the page's HTML is fully parsed
  --------------------------------------------------------------------------- */
  function init() {
    initFancybox();
    initSlideshows();
    initShuffle();
    initFootnoteBand();
  }

  if (document.readyState === 'loading') {
                                     /* HTML still being parsed (can happen if a
                                        script is injected early): wait for the
                                        DOMContentLoaded event. */
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();                          /* Already parsed (normal case for a script
                                        placed just before </body>): run now. */
  }

})();                                /* End of the IIFE, which calls itself. */


/* ==========================================================================
   Personal timeline: type filter (about.qmd, #timeline)

   The timeline itself needs no JavaScript; <details> handles expansion
   natively. This only shows and hides entries by type.
   ========================================================================== */
(function () {

  var wrap = document.querySelector('.tl-wrap');
  if (!wrap) return;                       // Every page that isn't about.html.

  var chips = wrap.querySelectorAll('.tl-chip');
  var items = wrap.querySelectorAll('.tl-item');
  var years = wrap.querySelectorAll('.tl-group');

  chips.forEach(function (chip) {
    chip.addEventListener('click', function () {

      var filter = chip.dataset.filter;

      chips.forEach(function (c) { c.classList.remove('is-active'); });
      chip.classList.add('is-active');

      // The `hidden` attribute is used rather than a class because it carries
      // the right meaning for assistive technology automatically.
      items.forEach(function (li) {
        li.hidden = (filter !== 'all' && li.dataset.type !== filter);
      });

      // A year header with nothing under it is visual noise, so hide any that
      // no longer have a visible entry. Walk forward from each header until
      // the next one, checking whether anything survived the filter.
      years.forEach(function (y) {
        var any = false;
        var n = y.nextElementSibling;
        while (n && !n.classList.contains('tl-group')) {
          if (!n.hidden) { any = true; break; }
          n = n.nextElementSibling;
        }
        y.hidden = !any;
      });
    });
  });

})();
