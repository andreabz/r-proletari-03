const tooltip = document.getElementById("tooltip-provincia");

document.querySelectorAll(".province").forEach(provincia => {
  provincia.addEventListener("mousemove", (e) => {
    const nome = provincia.getAttribute("data-nome");
    tooltip.textContent = nome;
    tooltip.style.display = "block";
    tooltip.style.left = (e.pageX + 10) + "px";
    tooltip.style.top = (e.pageY + 10) + "px";
  });

  provincia.addEventListener("mouseleave", () => {
    tooltip.style.display = "none";
  });

  provincia.addEventListener("click", () => {
    const url = provincia.getAttribute("data-url");
      if (url) {
        window.location.href = url;
      }
    });
  });
