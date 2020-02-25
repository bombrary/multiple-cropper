const outImages = (src, boxies) => {
  const img = new Image
  img.src = src
  img.onload = () => {
    const res = boxies.map((b) => {
      return clippedImageCanvas(img, b.x, b.y, b.width, b.height).toDataURL()
    });
    app.ports.receiveClippedImages.send(res);
  }
}

const clippedImageCanvas = (img, x0, y0, width, height) => {
  const canvas = document.createElement('canvas');
  canvas.width = width
  canvas.height = height
  const context = canvas.getContext('2d');
  context.drawImage(img, x0, y0, width, height, 0, 0, width, height);
  return canvas;
}

