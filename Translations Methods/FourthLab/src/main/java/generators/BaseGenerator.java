package generators;

import com.squareup.javapoet.JavaFile;

public interface BaseGenerator {
    JavaFile.Builder generate(String filePackage);
}
