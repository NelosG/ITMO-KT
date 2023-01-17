package ru.akirakozov.sd.refactoring.render.html;

import ru.akirakozov.sd.refactoring.render.html.parts.HtmlViewPart;

public abstract class HtmlMarkupRenderer extends HtmlRenderer {
    protected HtmlMarkupRenderer(HtmlViewPart... parts) {
        super(parts);
    }

    @Override
    public String render() {
        return "<html><body>" + super.render() + "</body></html>";
    }
}